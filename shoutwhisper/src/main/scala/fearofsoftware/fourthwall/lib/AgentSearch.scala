package fearofsoftware.fourthwall.lib

import fearofsoftware.fourthwall.model._
import fearofsoftware.fourthwall.comet.CometClient
import java.util.Date
import scala.collection.JavaConversions.JConcurrentMapWrapper
import java.util.concurrent.ConcurrentHashMap
import net.liftweb.actor.LiftActor
import fearofsoftware.fourthwall.comet.FosReceivedReset
import net.liftweb.mapper.By

/**
 * 
 */
object AgentSearch {
	
	/**
	 * These are immutable and will always forward to all users.
	 */
	val universalAgents = Set[Agent](
			
	)
	universalAgents.foreach(agent => {
		agent.self.save
		UniversalAgents.create.agentId(agent.getId).save
	})
	
	/**
	 * We could still use user actors if we had to, but maybe we don't.
	 */
	val userClients = new JConcurrentMapWrapper(new ConcurrentHashMap[User, (Set[CometClient], UserActor)])
	
	/*
	 * Logs the user out of that source/breaks the connection
	 * Also sends a message to the UI
	 */
	def sourceBroken(owners : Collection[DiscourseObject], source : Any) = {
		owners.foreach(owner => {
			
		})
	}
	
	/**
	 * How do we stop floods of one kind of message from entirely drowning a user?
	 * 
	 * In later stages, this would probably involve some kind of complex de-duplication scheme.  Very similar
	 * pieces of data would be compressed into a high-redundancy cluster store, lengthening history.
	 * 
	 * The simplest solution is to force hierarchy to act as a queue.
	 * 
	 * The next simplest solution is to break it up explicitly into per-agent hierarchies.
	 * 
	 */
	class UserActor(user : User) extends LiftActor{
		
		private var agents = scala.collection.immutable.Set[Agent]()
		
		private val hierarchy : Hierarchy = new Hierarchy(
			(d : DiscourseObject) => {
				agents.map(a => d.wordBagCompare(a.self)).sum
			}
		)
		
		private val clients = scala.collection.mutable.Set[CometClient]()
		
		override def messageHandler: PartialFunction[Any,Unit] = {
			case x : (Article, Set[Agent]) => {
				hierarchy.addLeaf(x._1.self)
				clients.foreach(_ ! FosReceivedReset(hierarchy))
			}
			case x : AddClient => {
				clients += x.client
				x.client ! FosReceivedReset(hierarchy)
			}
			case x : RemoveClient => {
				x.client ! FosReceivedReset(null)
				clients -= x.client
			}
			case Reset => {
				hierarchy.clear
				//when adding a new agent, should we build in that agent's history?
				//if we don't, it means that the user doesn't see things that might have come to the agent in between some things
				agents = user.getDiscourse.get[RelationTraitImpl]("agents").subjects.toSet
				//println("Recieved resest and now have agents " + agents)
				//println("Got reset with agents " + agents)
				hierarchy.addMany(user.getHistory.toList.take(DiscourseObject.maxHistory).map(_.self))
				clients.foreach(_ ! FosReceivedReset(hierarchy))
			}
		}
	}
	private case class AddClient(client : CometClient)
	private case class RemoveClient(client : CometClient)
	case object Reset
	
	/**
	 * Gets an article created by a watcher.  Sends it to the appropriate places,
	 * which are the agent histories and the registered clients.
	 * 
	 * If the article has private agents, it should only match to the per-user versions of
	 * those agents.  So it should be safe to match across all owners of agents that the
	 * agents contain as owners.
	 * 
	 * How do we deal with global/pre-initialized agents?
	 */
	def forward(article : Article) = {
		//println("forwarding " + article)
		val userAgentArticle = scala.collection.mutable.Map[User, Set[Agent]]()
		object userAgentArticleActor extends LiftActor{
			override def messageHandler: PartialFunction[Any,Unit] = {
				case x : (User, Agent) => {
					if(userAgentArticle.contains(x._1)){
						userAgentArticle.put(x._1, userAgentArticle(x._1) + x._2)
					}else{
						userAgentArticle.put(x._1, Set[Agent](x._2))
					}
				}
				case Dispatch => {
					userAgentArticle.foreach{
						case (user, agents) =>{
							//println("agents getting " + article.self.allTraits)
							agents.foreach(_.appendHistory(article))
							//println("saving " + article.self.allTraits)
							user.appendHistory(article, agents)
							//println("considering dispatching " + article.self.allTraits)
							if(userClients.contains(user)){
								//println("Dispatching " + article.self.allTraits)
								userClients(user)._2 ! (article, agents)
							}
						}
					}
				}
			}
		}
		case object Dispatch
		article.findRelevant.foreach((agent : Agent) => {
			//now we can get to forwarding to the clients
			//build a map from user to article by agent
			//a userless agent is totally global
			val owners = agent.self.get[RelationTraitImpl]("owners").subjects.toSet
			if(!owners.isEmpty) owners.foreach((owner : DiscourseObject) => {
				userAgentArticleActor ! (User.find(By(User.self, owner)).openTheBox, agent)
			}) else User.findAll.map(key => userAgentArticleActor ! (key, agent))
		})
		userAgentArticleActor ! Dispatch
	}
	
	/**
	 * Returns an actor which the client can use to ask for history.  If a user actor
	 * is already set up, then it may dispatch messages while the map is being modified,
	 * missing the new client.  However, those messages will appear in the history.  It
	 * might also proceed to send spurious messages, which is also bad.  Ideally, it
	 * becomes aware of the client only when the client has requested history.
	 * We remove the user actor from our table when its clients drop to 0, even if
	 * it still has some messages to dispatch - that means that new messages will
	 * get picked up be the next user object.
	 */
	def registerClient(client : CometClient) : LiftActor= {
		val user = User.find(By(User.id, client.owner)).openTheBox
		synchronized{
			if(!userClients.contains(user)){
				val actor = new UserActor(user)
				userClients.put(user, (Set[CometClient](client), actor))
				//it's possible that some messages will arrive before the reset
				//the actor must be tolerant of these
				//the reset guarantees that it doesn't miss a message between creation and registration
				actor ! Reset
				actor ! AddClient(client)
			}else{
				val old = userClients(user)
				userClients.put(user, (old._1 + client, old._2))
				old._2 ! AddClient(client)
			}
			userClients(user)._2.asInstanceOf[LiftActor]
		}
	}
	
	def deregisterClient(client : CometClient) : Unit = {
		val user = User.find(By(User.id, client.owner)).openTheBox
		synchronized{
			val old = userClients(user)
			val clients = old._1 - client
			if(clients.isEmpty){
				userClients.remove(user)
			}else{
				userClients.put(user, (clients, old._2))
				old._2 ! RemoveClient(client)
			}
		}
	}
}