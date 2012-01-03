package fearofsoftware.fourthwall.lib

import fearofsoftware.fourthwall.model.Article
import java.util.Date
import net.liftweb._
import net.liftweb.common.{Box, Full}
import net.liftweb.util._
import net.liftweb.actor._
import net.liftweb.util.Helpers._
import scala.xml._
import scala.collection.JavaConversions._
import fearofsoftware.fourthwall.model._
import org.scribe.exceptions.OAuthException


class CheckingActor[T, S](down : (T => List[S]), parse : ((S, Set[DiscourseObject]) => Article), entryDate : (S => Date)) extends LoadActor{

	private var deactivating = false
	private var activated = false
	private val sources = scala.collection.mutable.Map[T, Date]()
	private val owners = scala.collection.mutable.Map[T, Set[DiscourseObject]]()
	private val averageTimes = scala.collection.mutable.Map[T, Long]()
	private val messagesPerSource = scala.collection.mutable.Map[T, Long]()
	private var time = 60000000L
	private var minTime = 1000L
	private var maxTime = 60000000L
	
	private def checkForMessage = {
		//println("Making check with sources " + sources)
		sources.foreach{case (source, date) => {
			//println("At least attempting to check a source")
			try{
				val entries = down(source).filter(entry => (entryDate(entry) compareTo date) > 0)
				//println("Downed entries")
				/*
				 * Updates the average time between entries.
				 * 
				 * This enters special conditions if we have not yet had a message from this source
				 * (in which cause we must choose sane defaults) or if we 
				 */
				if(entries.size > 0){
					val newDates = entries.map(entryDate(_)).toList.sort((x : Date, y : Date) => 
						(x compareTo y) > 0).toArray
					if(messagesPerSource(source) > 0){
						//then we take the comparison between this and the previous message
						averageTimes(source) = 
							((averageTimes(source) * messagesPerSource(source)) + newDates(0).getTime - date.getTime) /
								(messagesPerSource(source) + 1)
						messagesPerSource(source) += 1
					}
					for(i <- 1 until newDates.length){
						averageTimes(source) = 
							((averageTimes(source) * messagesPerSource(source)) + newDates(i).getTime - newDates(i - 1).getTime) /
								(messagesPerSource(source) + 1)
						messagesPerSource(source) += 1
					}
				}
				//println("Filtered entries and processed times.")
				sources.put(source, entries.foldLeft(date)((acc : Date, entry : S) => {
					if((entryDate(entry) compareTo acc) > 0) entryDate(entry) else acc
				}))
				//println("added in new dates")
				//val articles = entries.map(parse(_, if (owners.contains(source)) owners(source) else null))
				//println("Found articles " + articles.map(_.self.allTraits))
				//articles.toArray.foreach(article => scala.actors.Futures.future(AgentSearch.forward(article)))
				//println("Article owners " + (if (owners.contains(source)) owners(source) else null))
				val fut = scala.actors.Futures.future(entries.toList.map((entry : S) => AgentSearch.forward(
						parse(entry, if (owners.contains(source)) owners(source) else null))))
			}catch{
				case e : OAuthException => {
					AgentSearch.sourceBroken(owners(source), source)
				}
			}
			//dunno if we do anything with these futures
		} case _ => println("Attempted to check malformed source")}
	}
	
	override def messageHandler: PartialFunction[Any,Unit] = {
		case x : AddSource[T] => {
			sources.put(x.source, new Date(0))
			//println("Adding source " + x.source.toString + " with owners " + x.owners)
			if(x.owners != null){
				owners.put(x.source, x.owners)
			}
			messagesPerSource.put(x.source, 0)
			averageTimes.put(x.source, 0)
		}
		case x : DeleteSource[T] => {
			sources.remove(x.source)
		}
		case ForceCheck => {
			checkForMessage
		}
		case Check => {
			checkForMessage
			if(activated){
				if(deactivating){
					activated = false
					deactivating = false
				}else{
					this ! ForceCheck
					//update timings with new averages
					//we should have minimum timings just in case
					time = if(averageTimes.isEmpty) time else
						List[Long](maxTime, List[Long](averageTimes.values.toSet.min, minTime).max).min
					
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
			minTime = x.time
		}
		case x : MinTime => {
			minTime = x.time
		}
		case x : MaxTime =>{
			maxTime = x.time
		}
		case x : SetDate[T] => {
			sources(x.source) = x.date
		}
	}
	
	private case object Check
	
	def load : Int = {
		sources.size
	}
}
case class Time(time : Long) extends Command
case class MinTime(time : Long) extends Command
case class MaxTime(time : Long) extends Command
case class AddSource[T](source : T, owners : Set[DiscourseObject]) extends Command
case class DeleteSource[T](source : T) extends Command
case class SetDate[T](source : T, date : Date) extends Command
case object ForceCheck


