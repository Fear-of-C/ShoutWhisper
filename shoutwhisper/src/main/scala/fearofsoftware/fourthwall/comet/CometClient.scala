package fearofsoftware.fourthwall.comet

import net.liftweb._
import http._
import SHtml._
import net.liftweb.common.{Box, Full}
import net.liftweb.util._
import net.liftweb.actor._
import net.liftweb.util.Helpers._
import scala.xml._
import scala.collection.JavaConversions._
import com.sun.syndication.io._
import com.sun.syndication.feed.synd._
import java.net.URL
import net.liftweb.http.js._
import js.jquery._
import js._
import js.JsCmds._
import JE._
import fearofsoftware.fourthwall.lib.input._
import fearofsoftware.fourthwall.lib._
import fearofsoftware.fourthwall.model.User
import java.util.Date
import scala.collection.JavaConversions.JConcurrentMapWrapper
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.ListBuffer
import fearofsoftware.fourthwall.model._
import fearofsoftware.fourthwall.snippet.cometClient

/**
 * Doesn't so much implement agents as serve as a way for the server to connect to one.  Not
 * 100% sure how this integrates with the client-side agents.
 * 
 * This class must keep its own mapping of agent to position.
 *  
 * @author nick
 *
 */
class CometClient extends CometActor{
	
	/**
	 * This gets sorted by date before pruning.  The messages that come in are also sorted by date.  Since we have enough
	 * entries to fill out our entire queue from each actor, we don't need to worry about getting enough from all of them.
	 * 
	 * Since the event-driven method also sorts by date and prunes, we should reach a stable state as long as all
	 * messages have been delivered.
	 * 
	 * There is a place, however, for historical instability in this list.  That's if an agent waits a very long
	 * time to send a very late message - its entire queue could come before that message, in which case a very
	 * old client would still have the message even though it should not be in agent histories.
	 * 
	 * In fact, this is generally a problem - it might be that a message is relatively unimportant to an agent
	 * but very important client-side. It might also be that some messages are arriving very late but due to
	 * date and/or importance should stay in the client.  This seems like a case for having user histories -
	 * as well as agent histories?  Yes, it would seem we need both.  If we just keep agent histories,
	 * then the client side may disagree about what's worth holding onto.  If we just keep user histories,
	 * then a particularly aggressive agent might drown the feed.  If we keep user histories but limit agent
	 * queues by number, we have the same problem as with agent histories - an agent might dump too much in
	 * and cause itself to lose place.
	 * 
	 * If we're not using importance, then we can almost rely on things coming in date order or almost in
	 * date order (close enough for the UI).  We will need to fix this soon.
	 * 
	 * Actually, we have a huge problem with agent queues being finite and recomputing importance when agents
	 * are added/removed.  Maybe we need indefinite or near-indefinite shelves?  Maybe we can keep separate
	 * data structures for what's come in between logins and what was important from before, and attempt to
	 * prove that something can be eliminated before garbage collection takes it into the aether.
	 */
	
	private var hierarchy : Hierarchy = null
	
	var searchText = ""
		
	/*
	 * Codes for things that will update RSS feeds.
	 */
	
	def ac = SHtml.ajaxCall(JsRaw("$('#query').val()"), updateCmd)
	
	def jsCode = "$('#query').keyup(function() {" + ac._2.toJsCmd + "});"
	
	def buttonCode = "$('#query').keyup(function() {" + ac._2.toJsCmd + "});"
	
	override def localSetup {
		super.localSetup()
		//Could we just hold off the FoSInit message until we get a first login?
		this ! FoSInit
	}
	override def localShutdown {
		if(owner > -1){
			AgentSearch.deregisterClient(this)
		}
		super.localShutdown()
	}
	
	override def defaultPrefix = Full("comet")
		
	def render = {
		"#client" #> <span id="client"> {computeVisible} </span><script type="text/javascript">{jsCode}</script>
	}
	
	def updateCmd(query : String) ={
		searchText = query
		SetHtml("client", computeVisible)
	}
	
	var owner = -1L
	
	def computeVisible : NodeSeq = {
		def getDate(d : DiscourseObject) = {
			if(d.has("date")) d.get[Date]("date") else d.get[Date]("date created")
		}
		val filteredEntries = if(hierarchy != null) 
			hierarchy.getRepresentatives(10).map(d => d.representatives(1).first).
				sort((d1 : DiscourseObject, d2 : DiscourseObject) => (getDate(d1) compareTo getDate(d2)) > 0).
				map(articleString(_))
			else Nil
		Unparsed(filteredEntries.toList.map({
					entry => "<p class=\"entry\">" + entry + "</p>"
					}).mkString("\n"))
	}		
	
	def articleString(article : Article) : NodeSeq = {
		<div class="row"><div class="col_12 omega"><article>{Unparsed(
			try{
			article("type") match{
				case ArticleType.NewsItem => {
						(if(article.has("author")) article("author") else "anon") +
						(if(article.has("link")) "<a href=\"" + article("link") + "\">" else "") +
						(if(article.has("title")) article("title") else "untitled") + 
					" | " + (if(article.has("link")) "</a>" else "") +
					article("date") 
				}
				case ArticleType.FBPost => {
						article("author").asInstanceOf[Article]("name") + " | " +
						(if(article.has("text"))  article("text") + " | " else "") +
						article("date") + " | " +
						article.get[RelationTraitImpl]("likes").subjects.size +
						article.get[RelationTraitImpl]("actions").subjects.map(
								act => "<a href=" + act("link") +">" + act("name") + "</a>").mkString(" | ")
				}
				case ArticleType.Tweet => {
						article("user").asInstanceOf[DiscourseObject]("name") + " | " +
						(if(article.has("text"))  article("text") + " | " else "") +
						article("created_at")
				}
				case _ => {
					"System Error"
				}
			}
		}catch{
			case _ => "System Error"
		})}</article></div></div>
	}
	
	def performUpdate = {
		reRender(false)
	}
	
	def externalInit = {
		this ! FoSInit
	}
	
	override def highPriority : PartialFunction[Any,Unit] = {
		case FoSInit => {
			User.currentUser match{
				case Full(user) => {
					owner = user.id.is
					AgentSearch.registerClient(this)
				}
				case _ => {
					//println("Didn't match a user.")
					cometClient.set(Box(this))
					owner = -1
				}
			}
		}
	}
	private case object FoSInit

	override def lowPriority : PartialFunction[Any,Unit] = {
		case x : FosReceivedReset => {
			hierarchy = x.hierarchy
			performUpdate
		}
	}
}
case class FosReceivedReset(hierarchy : Hierarchy)