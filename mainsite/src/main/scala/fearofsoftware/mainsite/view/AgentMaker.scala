package fearofsoftware.mainsite.view

import net.liftweb._
import http._

object AgentMaker extends LiftScreen{
	val name = field(S ? "Agent Name", "",
			trim, 
			valMinLen(2, "Name too short"),
			valMaxLen(40, "That's a long name"));
	

	//sources
	val sauce = field(S ? "Like chocalate sauce?", false)

	def finish() {
		S.notice("I like "+name.is+" too!")
	}
}