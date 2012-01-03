package fearofsoftware.fourthwall.lib
import net.liftweb.actor.LiftActor

/**
 * Actor can take on loads to check/ping at certain intervals.  Has methods for auto-tuning
 * this procedure.
 * 
 * Should be able to adjust timing for some optimality - note, however, that we may have both
 * global and local constraints on timing, and that not all sources assigned to a single actor
 * will necessarily have the same checking interval.
 * 
 * We certainly could assign each source its own actor.  Might not be much higher in overhead
 * than what we're already doing, unless it slows down ActorPing.  1 source per actor is
 * beginning to seem like not such a terrible idea, especially considering that we already
 * have 1 cometactor per user, which means we already have an actor active for every FB feed.
 * That said, if each person has a large # of sources, it could cause the # of actors to grow
 * very quickly compared to source counts.
 * 
 * Information that helps:
 * 1) Average interval between checks for each source.  This is the meat of timing heuristics -
 * it's what makes sure that we check at reasonable intervals.  For some sources (such as FB),
 * this might not mean much - others (like RSS) may have huge divides in checking time.
 * 2) How much load the entire system has on any given source, and how much load it has in
 * general.  We don't want to overload the Twitter API, and in the event of a usage explosion,
 * we want to make sure that we don't kill our own severs.
 */
trait LoadActor extends LiftActor{

	def load : Int
	
	override def messageHandler: PartialFunction[Any,Unit] = {
		case x : WrappedCommand => {
			messageHandler(x.c)
			x.sender ! CommandFinished(x.c, this)
		}
	}
}
case class Command
case class Start extends Command
case class Stop extends Command
case class WrappedCommand(c : Command, sender : LiftActor)
case class CommandFinished(c : Command, actor : LoadActor)