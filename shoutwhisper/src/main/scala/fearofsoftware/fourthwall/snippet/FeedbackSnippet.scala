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
import org.json.simple._
import org.json.simple.parser._
import net.liftweb.http.js._
import net.liftweb.mapper.By

object FeedbackSnippet extends DispatchSnippet{
	
	def dispatch() = {
		case _ => testBox
	}
	
	def testBox(html : NodeSeq) : NodeSeq = 
		<p>Hey there</p>
		/*<p>{SHtml.ajaxButton("Testing this button", () => {
				JsCmds.Alert("this button works")
						})}</p>*/
		
		//("id=agentDisplay" #> <div id="agentDisplay">{renderAgents}</div>)(html)
	
}