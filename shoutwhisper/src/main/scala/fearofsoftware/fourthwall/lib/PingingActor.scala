package fearofsoftware.fourthwall.lib

import net.liftweb._
import net.liftweb.common.{Box, Full}
import net.liftweb.util._
import net.liftweb.actor._
import net.liftweb.util.Helpers._

import fearofsoftware.fourthwall.model.Article
import fearofsoftware.fourthwall.model.DiscourseObject

class PingingActor(t : Long) extends LoadActor with LiftActor {

	private var message : Map[String, Any] = null
	private var deactivating = false
	private var activated = false
	var time = t
	
	override def messageHandler: PartialFunction[Any,Unit] = {
		case x : ChangeArticle => {
			message = x.message
		}
		case Check => {
			AgentSearch.forward(DiscourseObject.makeArticle(message))
			if(activated){
				if(deactivating){
					activated = false
					deactivating = false
				}else{
					ActorPing.schedule(this, Check, time)
				}
			}
		}
		case Start => {
			if(!activated){
				activated = true
				//begin checking immediately upon starting
				this ! Check
			}
		}
		case Stop => {
			if(activated){
				deactivating = true
			}
		}
		case x : Time => {
			time = x.time
		}
	}
	
	private case object Check
	
	def load : Int = {
		1
	}
}
case class ChangeArticle(message : Map[String, Any])