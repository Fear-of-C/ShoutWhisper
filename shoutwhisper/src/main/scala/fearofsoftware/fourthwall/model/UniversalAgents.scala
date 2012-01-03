package fearofsoftware.fourthwall.model
import net.liftweb.mapper._

/**
 * Simple table containing agents that listen for everything they're allowed to hear by privacy.
 */
class UniversalAgents extends LongKeyedMapper[UniversalAgents] with IdPK{

	def getSingleton = UniversalAgents
	
	object agentId extends LongMappedMapper(this, DiscourseObject)
}

object UniversalAgents extends UniversalAgents with LongKeyedMetaMapper[UniversalAgents]{
	
}