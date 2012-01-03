package fearofsoftware.fourthwall.lib

import net.liftweb._
import net.liftweb.common.{Box, Full}
import net.liftweb.util._
import net.liftweb.actor._
import net.liftweb.util.Helpers._
import java.util.Date
import scala.collection.JavaConversions._
import fearofsoftware.fourthwall.lib.input._
import scala.collection.mutable.PriorityQueue
import com.restfb.types._
import com.sun.syndication.feed.synd._
import com.restfb.DefaultFacebookClient
import com.restfb.types.Post
import com.sun.syndication.io.SyndFeedInput
import scala.collection.JavaConversions.JConcurrentMapWrapper
import fearofsoftware.fourthwall.model.Account
import net.liftweb.mapper.By
import scala.collection.mutable.HashMap
import fearofsoftware.fourthwall.model.DiscourseObject
import org.scribe._
import org.scribe.oauth._
import org.scribe.model._
import org.scribe.builder._
import org.scribe.builder.api._
import org.json.simple._
import org.json.simple.parser._
import fearofsoftware.fourthwall.snippet.LoginSnippet


/**
 * Manages distribution of tasks among actors.  Should handle the task of ensuring even distributions,
 * kind of like having an actor pool (which it may itself possess one day).
 * 
 * The reason for using this as an actor rather than a collection of synchronized maps is so that we
 * have full, synchronized control over the distribution of labor.
 * 
 * This class specifically distributes among actors that take timing information.  We can use checking
 * or pinging actors.
 * 
 * Factors to include in timing calcluations:
 * 1) A minimum interval based on the API's limitations.  This might be distributed if the limitation
 * is service-global.
 * 2) A maximum interval set based on the desire to ensure timely delivery of content.
 * 3) A minimum interval based on the ability of this system to handle high load, and based on the actor's
 * ability to process the number of streams (must usually clear current items before the next ping
 * arrives).
 * 4) An intelligent in-between heuristic.
 * 
 * As long as we must schedule globally, we should have a way to measure and set actor rates.  This will
 * probably come in the form of a map.
 * 
 * W/out having 1 actor/feed, we do not have per-source control.  This might encourage us to either
 * use a single checking actor for each feed, allow actors to schedule multiple ping intervals for
 * themselves, or group feeds into actors with similar timings.
 * 
 * The timing algorithm:
 * 1) Keep per-actor running tallies of sources and their update frequencies.
 * 2) Periodically call a check on all actors of these tallies.
 * 3) Redistribute using clustering on timings (this should not be too hard, since
 * we can sort timings into a list and then divvy up the chunks).
 * 4)  When a new source comes in, we can add it to a random actor or distribute to a special
 * place for measurement.
 * 5) We tally and redistribute at some interval determined by global load.  Actors can ping the
 * minidirector to notify that a certain load-like interval has passed, or we can simply check
 * periodically on a ping schedule.
 * 
 * We have to decide when to make redistributions.  we could make them on a set interval, such as
 * every 20 minutes, or we could trigger on certain events.
 * 
 * We absolutely must check loads when adding work, lest we accidentally go over global rate limits.
 * We also want to avoid wasting resources.
 * 
 * As it currently stands, this class lacks the capability to redistribute work among actors.  Maybe
 * we can compromise on this by having it merely re-time the existing actors.
 * 
 * We don't want to ping rare websites just because some website has an overactive RSS feed.
 * 
 * Note that the minload system doesn't make a whole lot of sense with these mechanics.  Maybe we
 * really should split into 1 actor per feed?  We can still keep the code for having multiple
 * feeds, but just not use it for now.  That way, ever actor can keep its own timings.
 * 
 * Each actor will act as an entry in a big timing table.  This does not seem like the worst
 * thing.
 * 
 * Actually, we can split RSS actors but let FB feeds share actors.  This way, most FB feeds
 * should have relatively similar statistics, but RSS feeds can move at something like
 * their own pace.
 */

class MiniDirector[T<:LoadActor](maxLoad : Int, globalMin : Long, localMin: (String => Long),
		defaultTime : Long, maxTime : Long, f : (Unit => T)) extends LiftActor{
	private val actorMap = scala.collection.mutable.Map[String, T]()
	private val ownerships = new scala.collection.mutable.HashMap[T, Set[String]](){
		override def default(key : T) = Set[String]()
	}
	private val actorSet = scala.collection.mutable.Set[T]()
	private val counts = scala.collection.mutable.Map[String, Long]()
	
	private var actorMinTime : Long = 1000L
	
	override def messageHandler: PartialFunction[Any,Unit] = {
		case x : AddWork => {
			if(actorMap.contains(x.id)){
				counts(x.id) += 1
			}else{
				counts.put(x.id, 1)
				var minLoad : T = if(!actorSet.isEmpty) actorSet.reduceLeft((a1 : T, a2 : T) => {
					if(a1.load < a2.load) a1 else a2
				}) else null.asInstanceOf[T]
				if((minLoad == null) || (minLoad.load >= maxLoad)){
					//make sure that we can't overload the feed
					minLoad = f()
					minLoad ! MaxTime(maxTime)
					//don't check more often than localmin
					minLoad ! MinTime(List[Long](localMin(x.id), actorMinTime).max)
					minLoad ! Time(defaultTime)
					minLoad ! x.message
					minLoad ! Start
					actorSet.add(minLoad)
				}else{
					minLoad ! x.message
					minLoad ! ForceCheck
				}
				actorMap.put(x.id, minLoad)
				ownerships.put(minLoad, ownerships(minLoad) + x.id)
				//now attempt to reset everyone's timing to ensure that we don't overrun any APIs
				actorMinTime = globalMin * (actorMap.size + 1)
				ownerships.foreach{case (actor, ids) => {
					actor ! MinTime((ids.map(localMin(_)).toSet + actorMinTime).max)
				}}
			}
		}
		case x : DeleteWork => {
			counts(x.id) -= 1
			if(counts(x.id) == 0){
				val act : T = actorMap.remove(x.id).getOrElse(null.asInstanceOf[T])
				act ! WrappedCommand(x.message, this)
				counts.remove(x.id)
			}
		}
		case x : CommandFinished => {
			x.c match {
				case y : DeleteSource[T] => {
					if(x.actor.load == 0){
						actorSet.remove(x.actor)
					}
				}
			}
		}
	}
}
case class AddWork(id : String, message : Command)
case class DeleteWork(id : String, message : Command)

/**
 * We could use synchronized blocks/methods instead of making this class its own actor.
 * 
 * The advantage of doing so would be removing the overhead of keeping an entire actor.
 * 
 * The disadvantage of doing so would be that these methods become blocking.
 * 
 * Right now, as there is no clear verdict, we can leave this as an actor and just
 * think about making sure that race conditions never occur.
 */
object Director extends LiftActor{
	
	/*
	 * Matches the name of the type of source to the necessary OAuth credentials
	 * Don't know if it should be in this object, but I needed to put it somewhere so Boot.scala could operate correctly
	 */
	val sourceTypes : scala.collection.mutable.HashMap[String, (Api, String, String, String)] = 
		new scala.collection.mutable.HashMap[String, (Api, String, String, String)]
		("twitter" -> (classOf[TwitterApi], LoginSnippet.twitterConsumerKey, LoginSnippet.twitterConsumerSecret, LoginSnippet.twitterRedirect))
	
	case class Source
	
	case class FBClient(client : DefaultFacebookClient) extends Source
	
	case class ScribeClient(service : OAuthService, at : Token) extends Source
	
	def toSource(sourceType : String, auth : String) : Source = {
		sourceType match{
			case "facebook" => FBClient(new DefaultFacebookClient(auth))
			/*
			 * Right now, if it's not Facebook, it's something using the Scribe API
			 */
			case _ => {
				def params = sourceTypes(sourceType)
				def newService = new ServiceBuilder()
                           .provider(params._1)
                           .apiKey(params._2)
                           .apiSecret(params._3)
                           .callback(params._4)
                           .build()
				
                /*
                 * Array of strings holding the Token and the Secret
                 */
                val arr = auth.split("/t")           
                
				ScribeClient(newService, new Token(arr(0), arr(1)))
			}
		}
	}
	
	private val clients = scala.collection.mutable.Map[String, Source]()
	
	//private val fbClients = scala.collection.mutable.Map[String, DefaultFacebookClient]()
	
	//private val twitterClients = scala.collection.mutable.Map[String, (OAuthService, Token)]()
	
	/**
	 * Facebook allows up to 50,000,000 API calls per day.  This is our total, global rate.
	 * 
	 * So if we had 1 FB page open, we could ping at an interval of 1.72800 requests per
	 * millisecond.  We can approximate this as 1.
	 * 
	 * There should be no local limit, other than that required by sanity.  Let's say 2
	 * minutes, since overchecking seems to mess things up.  Let's set a maximum checking interval of 10 minutes.
	 */
	
	
	/*
	 * Alex: we may want an algorithmic way of creating MiniDirectors
	 * We can do this by putting them in a Map (see messageHandler for Director class)
	 * 
	 * But we need to convert the Facebook MiniDirector to one that uses the Scribe OAuthService
	 */
	private val miniDirectors : scala.collection.mutable.HashMap[String, MiniDirector[CheckingActor[(OAuthService, Token), JSONObject]]] = new scala.collection.mutable.HashMap[String, MiniDirector[CheckingActor[(OAuthService, Token), JSONObject]]]()
	
	
	private val facebook = new MiniDirector[CheckingActor[DefaultFacebookClient, Post]](
			maxLoadFB, 1L, (String => 120000L), 120000L, 600000L, (Unit => 
				new CheckingActor[DefaultFacebookClient, Post](
						ArticleParser.downFB, ArticleParser.parseFB, ((post : Post) => 
							if(post.getUpdatedTime != null) post.getUpdatedTime else post.getCreatedTime)
				)
			)
	)
	
	private val twitter = new MiniDirector[CheckingActor[(OAuthService, Token), JSONObject]](
			maxLoadTwitter, 1L, (String => 120000L), 120000L, 600000L, (Unit => 
				new CheckingActor[((OAuthService, Token)), JSONObject](
						ArticleParser.downScribe(_, (Verb.GET, "https://api.twitter.com/1/statuses/home_timeline.json")), ArticleParser.parseScribe, ((obj : JSONObject) => 
							new Date(obj.get("created_at").asInstanceOf[String])
				)
			))
		)
	
	/**
	 * By default, wait at least 3 minutes before refreshing a website that uses RSS.
	 * 
	 * This number is taken from a random forum post about how often Google checks this thing.
	 */
	private val rssSites = new HashMap[String, Long](){
		override def default(s : String) = 300000L
	}
	
	/**
	 * RSS does not set hard limits as a global medium, but there might be local limits based on the
	 * websites we're pinging.  Maybe what we really need is a per-site limit?
	 * 
	 * By default, we check every 3 minutes.  At maximum, we check every hour.
	 */
	private val rss = new MiniDirector[CheckingActor[String, SyndEntry]](
			maxLoadFB, 0L, ((x : String) => rssSites(x)), 180000L, 360000L, (Unit => 
				new CheckingActor[String, SyndEntry](
						ArticleParser.downRSS, ArticleParser.parseRSS, ((entry : SyndEntry) => 
							if(entry.getUpdatedDate != null) entry.getUpdatedDate else entry.getPublishedDate)
				)
			)
	)
	
	def createTestActor = {
		val pAct = new PingingActor(1000L)
		pAct ! ChangeArticle(Map[String, Any](
			("text", "Test message / test message"),
			("owners", Set[DiscourseObject]()),
			("author", "Fear of Software"),
			("title", "Test message"),
			("description", "Test message"),
			("date published", new Date),
			("date", new Date),
			("source", "Fear of Software"),
			("type", ArticleType.NewsItem),
			("images", null),
			("link", "http://fearofsoftware.com/")
		))
		pAct ! Time(1000L)
		pAct ! Start
	}
	//createTestActor
	
	val predefinedRSS = Set[String](
			/*"http://feeds.bbci.co.uk/news/rss.xml",
			"http://feeds.bbci.co.uk/news/world/rss.xml",
			"http://feeds.bbci.co.uk/news/business/rss.xml",
			"http://feeds.bbci.co.uk/news/politics/rss.xml",
			"http://feeds.bbci.co.uk/news/health/rss.xml",
			"http://feeds.bbci.co.uk/news/education/rss.xml",
			"http://feeds.bbci.co.uk/news/science_and_environment/rss.xml",
			"http://feeds.bbci.co.uk/news/technology/rss.xml",
			"http://feeds.bbci.co.uk/news/entertainment_and_arts/rss.xml"*/
	)
	
	def maxLoadRSS = 1
	def maxLoadFB = 25
	def maxLoadTwitter = 25
	
	predefinedRSS.foreach(addRSSFeed(_))
	
	/**
	 * Adds to the database and then 
	 */
	def addRSSFeed(url : String) = {
		rss ! AddWork(url, AddSource[String](url, fearofsoftware.fourthwall.model.User.currentUser match {
			case Full(user) => Set[DiscourseObject](user.getDiscourse)
			case _ => null
		}))
	}
	
	def removeRSSFeed(url : String) = {
		rss ! DeleteWork(url, DeleteSource[String](url))
	}
	
	/*
	 * Use messages any time we need to avoid making a block that
	 */
	override def messageHandler: PartialFunction[Any,Unit] = {
		
		case x : AddSourceClient => {
			if (!clients.contains(x.id)){
				clients.put(x.id, x.source)
				x.source match{
					case y : FBClient => {
						facebook ! AddWork(x.id, AddSource[DefaultFacebookClient](y.client, Account.findAll(By(Account.accountId, x.id)).flatMap(_.users).map(_.getDiscourse).toSet))
					}
					/*
					 * Adding new sources:
					 * 	Right now all sources that are not Facebook use the tuple (OAuthService, Token)]
					 *  We can generalize this algorithm by having a Map of MiniDirector instances that correspond to strings
					 *  We can then simply have the "addSourceClient" method have some place where it defines the type of account
					 *  that it is
					 *  
					 * Want to change this method so that SQL call finds account by BOTH accountId and accountType
					 */
					case y : ScribeClient => {
						if (miniDirectors.keySet.contains(x.sourceType))
							miniDirectors(x.sourceType) ! AddWork(x.id, AddSource[(OAuthService, Token)]((y.service, y.at), Account.findAll(By(Account.accountId, x.id)).flatMap(_.users).map(_.getDiscourse).toSet))
						else
							println("Error at Director.scala: " + x.sourceType + " is not a valid MiniDirector")
					}
				}
			}
		}
		
		/*case x : AddFB => {
			if(!fbClients.contains(x.id)){
				//TODO: consider whether to replace existing access token
				//println("add facebook " + x.id)
				fbClients.put(x.id, x.at)
				facebook ! AddWork(x.id, AddSource[DefaultFacebookClient](x.at, Account.findAll(By(Account.accountId, x.id)).flatMap(_.users).map(_.getDiscourse).toSet))
			}
		}
		case x : RemoveFB => {
			//println("remove fb from " + fbClients.keys)
			//println("remove facebook " + x.id)
			facebook ! DeleteWork(x.id, DeleteSource[DefaultFacebookClient](fbClients(x.id)))
			fbClients.remove(x.id)
			println("this thing getting sent?")
		}
		
		case x : AddTwitter => {
			if (!twitterClients.contains(x.id)){
				twitterClients.put(x.id, x.at)
				twitter ! AddWork(x.id, AddSource[(OAuthService, Token)](x.at, TwitterAccount.findAll(By(TwitterAccount.twitterId, x.id)).flatMap(_.users).map(_.getDiscourse).toSet))
			}
		}
		
		case x : RemoveTwitter => {
			twitter ! DeleteWork(x.id, DeleteSource[(OAuthService, Token)](twitterClients(x.id)))
			twitterClients.remove(x.id)
		}*/
		
	}
	private case class AddSourceClient(id : String, sourceType: String, source : Source)
	private case class RemoveSourceClient(id : String)
	
	/*private case class AddFB(id : String, at : DefaultFacebookClient)
	private case class RemoveFB(id : String)
	private case class AddTwitter(id : String, at : (OAuthService, Token))
	private case class RemoveTwitter(id : String)*/
	
	
	def addSourceClient(id : String, sourceType : String, source: Source) : Unit = {
		this ! AddSourceClient(id, sourceType, source)
	}
	
	def removeSourceClient(id : String) = {
		this ! RemoveSourceClient(id)
	}
	
	/*
	 * Helper methods
	 * We can always get rid of these later if they prove too redundant
	 */
	
	//def addFB(id : String, at : DefaultFacebookClient) : Unit = addSourceClient(id, "facebook", FBClient(at))
	
	//def addFB(id : String, at : String) : Unit = addFB(id, new DefaultFacebookClient(at))
	
	/*def removeFB(id : String) = {
		this ! RemoveFB(id)
	}*/
	
	/*
	 * We MIGHT need to change this so that only the token is stored by the variable "at"
	 * and have a new OAuthService instance be made with each user login
	 * Not sure yet though
	 */
	/*def addTwitter(id : String, at : (OAuthService, Token)) : Unit = {
		this ! AddTwitter(id, at)
	}
	
	def removeTwitter(id : String) = {
		this ! RemoveTwitter(id)
	}*/
}
