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
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds.SetHtml

object ManagementSnippet extends DispatchSnippet{
	def dispatch() = {
		case "sources" => sources _
		case "agents" => agents _
	}
	
	def sources(html : NodeSeq) : NodeSeq = {
		User.currentUser match{
			case Full(user) => {}
			case _ => return <div>Log in to add sources to search for relevant updates.</div>
		}
		
		var url : String = ""
		var priv : Boolean = false
		/**
		 * This is where we might eventually put an autocomplete for RSS
		 * feeds.
		 */
		def onTextUpdated = {
			
		}
		
		def renderSources : NodeSeq = {
			User.currentUser match {
				case Full(user) => {
					
					val onlineAccounts = user.accounts.map(acc => {
						val deleteId = "deleteAccount" + acc.accountId.is
						<div class="row"><div class="col_12 omega">Account:
						{SHtml.ajaxButton("Remove", () => {
							Director.removeSourceClient(acc.accountId.is)
							user.accounts -= acc
							acc.delete_!
							user.save
							JsCmds.JsHideId(deleteId)
						}, ("id", deleteId))}</div></div>
					})
					
					/*val fbAlready = user.facebooks.map(fb => {
						val deleteId = "deletefb" + fb.fbId.is
						<p>Facebook Account: 
						{SHtml.ajaxButton("Remove", () => {
							Director.removeFB(fb.fbId.is)
							user.facebooks -= fb
							fb.delete_!
							user.save
							JsCmds.JsHideId(deleteId)
							
						}, ("id", deleteId))}</p>
						
					})*/
					
					/*val twitterAlready = user.twitters.map(tw => {
						val deleteId = "deleteTw" + tw.twitterId.is
						<p>Twitter Account: 
						{SHtml.ajaxButton("Remove", () => {
							Director.removeTwitter(tw.twitterId.is)
							user.twitters -= tw
							tw.delete_!
							user.save
							JsCmds.JsHideId(deleteId)
						}, ("id", deleteId))}</p>
					})*/
					
					val feeds = user.feeds.map(f => {
						val deleteId = "deleteRSS" + f.url.is
						<div class="row"><div class="col_12 omega">RSS Feed:
						{SHtml.ajaxButton("Remove " + f.url.is, () => {
							Director.removeRSSFeed(f.url.is)
							user.feeds -= f
							//f.users -= user
							user.save
							if(f.users.isEmpty){
								f.delete_!
							}
							JsCmds.JsHideId(deleteId)
						}, ("id", deleteId))}</div></div>
					})
					<span>{onlineAccounts}</span>
					<span>{feeds}</span>
				}
				case _ => {
					<span>You aren't logged in.</span>
				}
			}
		}
		
		/**
		 * Method that actually adds a new RSS feed.
		 * 
		 * 1st validate, then add.
		 * 
		 * We need to make sure it's a valid URL and then run it through ROME.
		 * 
		 */
		def processServer : JsCmd = {
			try{
				ArticleParser.downRSS(url)
				//if that worked, then the feed is probably valid
				val entry : RSSSource = RSSSource.find(By(RSSSource.url, url)) match{
					case Full(entry) => entry
					case _ => RSSSource.create.url(url).privacy(priv) 
				}
				User.currentUser match{
					case Full(user) => {
						//TODO: figure out if we really need all these save statements
						entry.save
						user.feeds += entry
						//entry.users += user
						user.save
						entry.save
						//entry.save
						//println("full user case: " + user.feeds)
						Director.addRSSFeed(url)
					}
					case _ => {
						//if not logged in, then don't bother databasing it 
						//println("etc case: " + User.currentUser.openTheBox.feeds)
					}
				}
				
				SetHtml("sourceDisplay", renderSources)
			}catch{
				case x : Exception => {
					S.notice("Couldn't parse RSS feed from: " + url)
					SetHtml("sourceDisplay", renderSources)
				}
			}
		}
		//("name=privacy" #> (SHtml.checkbox(priv, priv = _)) &
		(("id=deleteButton" #> (User.currentUser match {
			case Full(user) => SHtml.ajaxButton("Delete Shout/Whisper Account", () => {
						User.logUserOut
						
						/*
						 * Delete RSS feeds
						 */
						val rss = user.feeds.toSet
						user.feeds.clear
						rss.foreach(r => {
							if((r.users.map(_.id.is).toSet - user.id).isEmpty){
								Director.removeRSSFeed(r.url.is)
								r.delete_!
							}
						})
						
						/*
						 * Delete internet accounts
						 */
						val accounts = user.accounts.toSet
						user.accounts.clear
						accounts.foreach(acc => {
							if ((acc.users.map(_.id.is).toSet - user.id).isEmpty){
								Director.removeSourceClient(acc.accountId.is)
								acc.delete_!
							}
						})
						
						/*val facebooks = user.facebooks.toSet
						user.facebooks.clear
						val twitters = user.twitters.toSet
						user.twitters.clear
						//delete accounts
						facebooks.foreach(fb => {
							if((fb.users.map(_.id.is).toSet - user.id).isEmpty){
								Director.removeFB(fb.fbId.is)
								fb.delete_!
							}
						})
						twitters.foreach(tw => {
							if((tw.users.map(_.id.is).toSet - user.id).isEmpty){
								Director.removeTwitter(tw.twitterId.is)
								tw.delete_!
							}
						})*/
						
						//delete agents
						val agents = user.getDiscourse.get[RelationTraitImpl]("agents")
						val agentSet = agents.subjects.toSet
						agents.subjects.clear
						agentSet.foreach(_.delete_!)
						//delete actual user object
						user.getDiscourse.delete_!
						user.delete_!
						
						new JE.JsRaw("""window.location = "/";""").cmd
					}, ("id", "fullDeleteButton"))
			case _ => <span></span>
		})
					) &
		"name=location" #> (SHtml.text(url, url = _)
				++ SHtml.hidden(() => processServer)) &
		"id=sourceDisplay" #> <div id="sourceDisplay">{renderSources}</div>)(html)
	}
	
	def agents(html : NodeSeq) : NodeSeq = {
		User.currentUser match{
			case Full(user) => {}
			case _ => return <div>Log in to create agents that search for content relevant to queries you've entered.</div>
		}
		var query : String = ""
		var priv : Boolean = false
		var universal : Boolean = false
		var agents = scala.collection.mutable.Map[Agent, (String, Boolean)]()
		agents ++= (User.currentUser match{
			case Full(user) => {
				user.getDiscourse.get[RelationTraitImpl]("agents").subjects.map(sub => {
					(sub, (sub.get[String]("text"), 
						UniversalAgents.findAll(By(UniversalAgents.agentId, sub.id.is)) match{
							case Nil => false
							case _ => true
						}))
				})
			}
			case _ => Map[Agent, (String, Boolean)]()
		})
		/*
		 * 
		 * agents.map{case (agent, data) => {
				<div id={agent.self.id.is.toString}>{data._1.toString} {if(data._2) "universal" else ""}
				{SHtml.ajaxButton("Delete", () => {
					agents.remove(agent)
					agent.self.delete_!
					JsCmds.SetHtml("agentDisplay" , renderAgents)
				})}
				</div>
		 * 
		 */
		def renderAgents : NodeSeq = {
			val xml = <div id="agents">
				{for (entry <- agents) yield 
					<div id={entry._1.self.id.is.toString} class="row .agent"><div class="col_12 omega">{
					//println(entry)
					Unparsed("Agent: " + entry._2._1 + " " + (if(entry._2._2) "<b>(universal)</b>" else ""))}
					{SHtml.ajaxButton("Delete", () => {
						val agent = entry._1
						//TODO: figure out how ByList works and fix this bit of slowness
						val uo = agent.get[RelationTraitImpl]("owners").subjects.toSet
						val users = uo.flatMap(user => 
							User.findAll(By(User.self, user.id.is)))
						UniversalAgents.findAll(By(UniversalAgents.agentId, agent.self.id.is)).forall(_.delete_!)
						uo.foreach(u => {
							val agentRel = u.get[RelationTraitImpl]("agents").subjects
							agentRel  -= agent.self
							agentRel.save
						})
						agent.self.delete_!
						//System.out.println("Should have deleted agent " + agent)
						users.foreach(AgentSearch.userClients(_)._2 ! AgentSearch.Reset)
						agents.remove(agent)
						JsCmds.SetHtml("agentDisplay" , renderAgents)
						})}
					</div></div>
				}</div>
			//println(xml.toString)
			xml
		}
		
		def addAgent : JsCmd = {
			//println("Agent added.")
			User.currentUser match{
				case Full(user) => {
					//println("Adding agent: " + query)
					val agent = DiscourseObject.makeAgent(Map[String, Any](), query, Set[DiscourseObject](user.getDiscourse))
					agents.put(agent, (query, universal))
					if(universal){
						UniversalAgents.create.agentId(agent.getId).save
					}
					AgentSearch.userClients(user)._2 ! AgentSearch.Reset
					//println("creating agent in new snippet (Full case)")
					//println("universal: " + universal)
					JsCmds.SetHtml("agentDisplay" , renderAgents)
					//S.notice("Added Agent listening for: " + query)
				}
				case _ => {
					//println("creating agent (etc case)")
					JsCmds.SetHtml("agentDisplay", <span>You are not logged in.</span>)
					//S.notice("Added Agent listening for: " + query)
				}
			}
		}
		
		/*
		 * Some hacky unit testing
		 */
		/*User.currentUser match{
		  case Full(user) => {DiscourseObject.makeAgent(Map[String, Any](), "illusion", Set[DiscourseObject](user.getDiscourse))
		  DiscourseObject.makeAgent(Map[String, Any](), "a", Set[DiscourseObject](user.getDiscourse))
		  DiscourseObject.makeAgent(Map[String, Any](), "new", Set[DiscourseObject](user.getDiscourse))
		  DiscourseObject.makeAgent(Map[String, Any](), "iPhone", Set[DiscourseObject](user.getDiscourse))
		  DiscourseObject.makeAgent(Map[String, Any](), "home", Set[DiscourseObject](user.getDiscourse))
		  DiscourseObject.makeAgent(Map[String, Any](), "in", Set[DiscourseObject](user.getDiscourse))
		  DiscourseObject.makeAgent(Map[String, Any](), "I", Set[DiscourseObject](user.getDiscourse))
		  DiscourseObject.makeAgent(Map[String, Any](), "what", Set[DiscourseObject](user.getDiscourse))
		  DiscourseObject.makeAgent(Map[String, Any](), "is", Set[DiscourseObject](user.getDiscourse))
		  DiscourseObject.makeAgent(Map[String, Any](), "this", Set[DiscourseObject](user.getDiscourse))
		  DiscourseObject.makeAgent(Map[String, Any](), "that", Set[DiscourseObject](user.getDiscourse))
		  }
		  case _ => println("oops, not logged in")
		}*/
		
		
		("name=universal" #> (SHtml.checkbox(universal, universal = _)) &
		"name=query" #> (SHtml.text(query, query = _) ++ SHtml.hidden(() => addAgent)) &
		//"name=add" #> (SHtml.ajaxButton("Add", () => addAgent)) &
		"id=agentDisplay" #> <div id="agentDisplay">{renderAgents}</div>)(html)
	}
}