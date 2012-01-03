package fearofsoftware.fourthwall.snippet

import scala.xml._
import net.liftweb._
import common._
import http._
import LiftRules._
import util.Helpers._
import net.liftweb.util._
import java.net.URL
import fearofsoftware.fourthwall.lib._
import fearofsoftware.fourthwall.comet._
import fearofsoftware.fourthwall.model._
import com.restfb.DefaultFacebookClient
import java.util.Date
import scala.util.Random
import org.scribe._
import org.scribe.oauth._
import org.scribe.model._
import org.scribe.builder._
import org.scribe.builder.api._
import fearofsoftware.fourthwall.lib.input.ArticleParser
import net.liftweb.mapper.By
import net.liftweb.http.js.JsCmds.SetHtml

object SourceSnippet extends DispatchSnippet{
	def dispatch() = {
		case "rss" => rssForm _
		case "createAgent" => createAgent _
	}
	
	/**
	 * Generates a form to create an agent.
	 */
	def createAgent(html : NodeSeq) : NodeSeq = {
		
		var query : String = ""
		var priv : Boolean = false
		var universal : Boolean = false
		
		def onTextUpdated = {
			
		}
		
		/**
		 * Method that actually adds a new agent.
		 * 
		 */
		def processServer(){
			User.currentUser match{
				case Full(user) => {
					val agent = DiscourseObject.makeAgent(Map[String, Any](), query, Set[DiscourseObject](user.getDiscourse))
					if(universal){
						UniversalAgents.create.agentId(agent.getId).save
					}
					AgentSearch.userClients(user)._2 ! AgentSearch.Reset
					println("creating agent (Full case)")
					println("universal: " + universal)
					SetHtml("agentMessages" , <span>New {if(universal) "universal" else ""} agent created and listening for: {query}</span>)
					//S.notice("Added Agent listening for: " + query)
				}
				case _ => {
					println("creating agent (etc case)")
					SetHtml("agentMessages", <span>You are not logged in.</span>)
					//S.notice("Added Agent listening for: " + query)
				}
			}
		}
		
		("name=universal" #> (SHtml.checkbox(universal, universal = _)) &
		"name=query" #> (SHtml.text(query, query = _)
				++ SHtml.hidden(processServer)))(html)
	}
	
	/**
	 * Should return HTML for adding RSS feeds.
	 * 
	 * See http://demo.liftweb.net/ajax for AJAX examples with Lift.
	 */
	def rssForm(html : NodeSeq) : NodeSeq = {
		
		var url : String = ""
		var priv : Boolean = false
		
		/**
		 * This is where we might eventually put an autocomplete for RSS
		 * feeds.
		 */
		def onTextUpdated = {
			
		}
		
		/**
		 * Method that actually adds a new RSS feed.
		 * 
		 * 1st validate, then add.
		 * 
		 * We need to make sure it's a valid URL and then run it through ROME.
		 * 
		 */
		def processServer(){
			try{
				ArticleParser.downRSS(url)
				//if that worked, then the feed is probably valid
				val entry : RSSSource = RSSSource.find(By(RSSSource.url, url)) match{
					case Full(entry) =>{
						if((priv && entry.privacy) || !(priv || entry.privacy)){
							entry
						}else{
							RSSSource.create.url(url).privacy(priv)
						}
					}
					case _ => RSSSource.create.url(url).privacy(priv) 
				}
				User.currentUser match{
					case Full(user) => {
						//TODO: figure out if we really need all these save statements
						entry.save
						user.feeds += entry
						//entry.users += user
						user.save
						//entry.save
						//println("full user case: " + user.feeds)
					}
					case _ => {
						//if not logged in, then don't bother databasing it unless it's public
						if(!entry.privacy.is){
							entry.save
						}
						//println("etc case: " + User.currentUser.openTheBox.feeds)
					}
				}
				Director.addRSSFeed(url)
				S.notice("Added RSS feed: " + url)
			}catch{
				case x : Exception => {
					S.notice("Couldn't parse RSS feed from: " + url)
				}
			}
		}
		("name=privacy" #> (SHtml.checkbox(priv, priv = _)) &
		"name=location" #> (SHtml.text(url, url = _)
				++ SHtml.hidden(processServer)))(html)
	}

}