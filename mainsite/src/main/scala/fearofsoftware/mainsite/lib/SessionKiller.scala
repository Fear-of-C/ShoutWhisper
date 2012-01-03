package fearofsoftware.mainsite.lib

import _root_.net.liftweb._
import http._
import util._
import Helpers._
import common._

object SessionKiller extends Function2[Map[String, SessionInfo],
                                        SessionInfo => Unit, Unit]{
	
	def defaultKillWhen = 180000L
	@volatile var killWhen = defaultKillWhen
	@volatile var killCnt = 1
	
	 def apply(sessions: Map[String, SessionInfo],
            destroyer: SessionInfo => Unit): Unit = {
		
		val cutoff = millis - 180000L
		 
		 sessions.foreach {
	      case (name, si @ SessionInfo(session, agent, _, cnt, lastAccess)) =>
	        if (cnt <= killCnt && lastAccess < cutoff) {
	          destroyer(si)
	        }
	    }
		 
	 }
}