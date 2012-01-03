package fearofsoftware.fourthwall.lib.input

import java.net.URL
import de.l3s.boilerpipe.extractors._
import com.restfb._
import com.restfb.types._
import java.util.Date
import com.sun.syndication.io._
import com.sun.syndication.feed.synd._
import scala.collection.JavaConversions._
import fearofsoftware.fourthwall.lib._
import fearofsoftware.fourthwall.model._
import net.liftweb.mapper._
import fearofsoftware.fourthwall.model.Article
import org.scribe._
import org.scribe.oauth._
import org.scribe.model._
import org.scribe.builder._
import org.scribe.builder.api._
import org.json.simple._
import org.json.simple.parser._
import net.liftweb.common.Full

object Privacy{
	val priv = 0
}

object ArticleType{
	val FBPost = 0
	val Tweet = 1
	val NewsItem = 2
}

object ArticleParser{
	
	def extractBody(addr: URL, cType: Int) = { 
		cType match{
		case ArticleType.NewsItem => ArticleExtractor.getInstance().getText(addr)
		case _ => DefaultExtractor.getInstance().getText(addr)
		}
	}
	
	
	/*
	 * We can generalize the downTwitter method to work for anything that is compatible with Scribe (which is anything, actually)
	 * We would just need an argument for what to enter into the OAuthRequest; maybe a few other details
	 */
	def downScribe(servTup : (OAuthService, Token), reqTup : (Verb, String) ) : List[JSONObject] = {
		val pollRequest : OAuthRequest = new OAuthRequest(reqTup._1, reqTup._2)   //(Verb.GET, "https://api.twitter.com/1/statuses/home_timeline.json")
		val service = servTup._1
		val accessToken = servTup._2
		service.signRequest(accessToken, pollRequest)
		val pollResponse : Response = pollRequest.send()
		val body = pollResponse.getBody()
		
		val obj=JSONValue.parse(body)
		
		var l : List[JSONObject] = List[JSONObject]()
		
		
		obj.asInstanceOf[JSONArray].toArray.toList.foreach{e => 
			if (e.isInstanceOf[JSONObject])
				l ::= e.asInstanceOf[JSONObject]
			else
				println("this probably shouldn't happen")
		}
		
		l
	}
	
	def parseScribe(entry : JSONObject, theUsers : Set[DiscourseObject]) = {
		
		var traits : Map[String, Any] = parseJSONObject(entry).asInstanceOf[Map[String, Any]]
		traits += (("owners", theUsers))
		traits += (("privacy", Privacy.priv))
		traits += (("type", ArticleType.Tweet))
		
		DiscourseObject.makeArticle(traits)
		
	}
	
	private def parseJSONObject(o : JSONObject) : Map[String, Any] = {
		var traits = Map[String, Any]()
		
			val entries = o.keySet.toArray.toList
		
			entries.foreach{ t =>
				val tVal = o.get(t)
				if (tVal.isInstanceOf[JSONObject])
					traits += ((t.toString, DiscourseObject.make(parseJSONObject(tVal.asInstanceOf[JSONObject]))))
				else if (tVal.isInstanceOf[JSONArray])
					traits += ((t.toString, DiscourseObject.make(parseJSONArray(tVal.asInstanceOf[JSONArray]))))
				else{
					traits += ((t.toString, tVal))
				}
			}
			
			traits
	}
	
	private def parseJSONArray(o : JSONArray) : Map[String, Any] = {
		var elts = Map[String, Any]()
			val l = o.toArray
			Range(0, l.size).foreach{i =>
				if (l(i).isInstanceOf[JSONObject])
					elts += ((i + "", parseJSONObject(l(i).asInstanceOf[JSONObject])))
				else
					elts += ((i + "", l(i)))
			}

			elts
	}
	
	private def parseJSON(entry : Any) : Any = {
		
			if (entry.isInstanceOf[JSONObject])
				parseObject(entry.asInstanceOf[JSONObject])
			else if (entry.isInstanceOf[JSONArray])
				parseArray(entry.asInstanceOf[JSONArray])
			else
				entry
		
		
		
		def parseObject(o : JSONObject) : Map[String, Any] = {
			
			var traits = Map[String, Any]()
		
			val entries = o.keySet.toArray.toList
		
			entries.foreach{ t =>
				val tVal = o.get(t)
				if (tVal.isInstanceOf[JSONObject])
					traits += ((t.toString, DiscourseObject.make(parseObject(tVal.asInstanceOf[JSONObject]))))
				else if (tVal.isInstanceOf[JSONArray])
					traits += ((t.toString, parseArray(tVal.asInstanceOf[JSONArray])))
				else
					traits += ((t.toString, tVal))
			}
			
			traits
		}
			
			
		
		def parseArray(o : JSONArray) : List[Any] = {
			var elts = List[Any]()
			val l = o.toArray.toList
			l.foreach{i =>
				if (i.isInstanceOf[JSONObject])
					elts ::= parseObject(i.asInstanceOf[JSONObject])
				else
					elts ::= i
			}
			elts
		}
		
		
	}

	def downFB(client : DefaultFacebookClient) = {
		/*
		 * me/home
		 */
		client.fetchConnection("me/home", classOf[Post]).getData().toList
	}

	def parseFB(entry : Post, fbUsers : Set[DiscourseObject]) = {
		
		def makeNamed(p : NamedFacebookType) : Article = {
			DiscourseObject.makeArticle(Map[String, Any](
					("name", p.getName),
					("fb_id", p.getId)
			))
		}
			
		def makeCommentArticle(c : Comment) : Article = {
			DiscourseObject.makeArticle(Map[String, Any](
					("date", c.getCreatedTime),
					("author", makeNamed(c.getFrom)),
					("comment-likes", c.getLikes),
					("text", c.getMessage)
			))
		}
		
		def makeAction(a : Post.Action) : Article = {
			DiscourseObject.makeArticle(Map[String, Any](
					("name", a.getName),
					("link", a.getLink)
			))
		}
		
		var traitList = 
				("type", ArticleType.FBPost) ::
				("owners", fbUsers) ::
				("privacy", Privacy.priv) ::
				("text" , entry.getMessage) ::
				("actions", entry.getActions().map(makeAction(_)).toList) ::
				("likes", if((entry.getLikes != null) && 
						(entry.getLikesCount != null) && 
						(entry.getLikesCount.longValue > 0)) entry.getLikes.getData.map(makeNamed(_)).toList else Nil) ::
				("text", entry.getMessage) ::
				("comments", entry.getComments.getData.map(comment => 
					makeCommentArticle(comment)).toList) ::
				("author", makeNamed(entry.getFrom)) ::
				("icon", entry.getIcon) ::
				("date", if (entry.getUpdatedTime != null) entry.getUpdatedTime() else 
					if (entry.getCreatedTime != null) entry.getCreatedTime else new Date) ::
				("date published", entry.getCreatedTime) ::
				Nil
			if(entry.getPicture != null){
				traitList ::= ("picture", entry.getPicture)
			}
			if(entry.getCaption != null){
			}
			if(entry.getSource != null){
				traitList ::= ("media", entry.getSource())
			}
			if(entry.getLink != null){
				traitList ::= ("link", entry.getLink)
				traitList ::= ("link caption", entry.getCaption)
				traitList ::= ("link description", entry.getDescription)

			}
			val d = DiscourseObject.makeArticle(traitList.toMap)
			//println("Created fb article with users " + d.get[RelationTraitImpl]("owners").subjects + " from " + fbUsers)
			d
	}
	
	def downRSS(url : String) = {
		val feedIn = new SyndFeedInput
		feedIn.build(new XmlReader(new URL(url))).getEntries().asInstanceOf[java.util.List[SyndEntry]].toList
	}
	
	def parseRSS(entry : SyndEntry, owners : Set[DiscourseObject]) = {
		//println("parsing rss article" + entry.getTitle)
		val d = DiscourseObject.makeArticle(Map[String, Any](
			("owners", owners),
			/*("privacy", RSSSource.find(By(RSSSource.url, entry.getSource().getUri())) match{
				case Full(rss) => if(rss.privacy.is) Privacy.priv else null
				case _ => null
			}),*/
			("text", ArticleParser.extractBody(new URL(entry.getLink), ArticleType.NewsItem)),
			("author", entry.getAuthor),
			("title", entry.getTitle),
			("description", entry.getDescription),
			("date published", entry.getPublishedDate),
			("date", if(entry.getUpdatedDate != null) entry.getUpdatedDate else
				if(entry.getPublishedDate != null) entry.getPublishedDate else new Date),
			("categories", entry.getCategories),
			("source", entry.getSource),
			("type", ArticleType.NewsItem),
			("images", null),
			("link", entry.getLink)
			))
		d
		//println("parsed RSS article: " + d.allTraits + " - " + d)
		
	}

}
