package fearofsoftware.mainsite.snippet

import net.liftweb._
import http._
import scala.xml._
import common._
import util.Helpers._
import js._
import js.JsCmds._
import JE._

import scala.util.Random


/**
 * Generates the A/B testing tree.
 * 
 * Ideally we'd have a CMS here.  The world is not ideal.  We will instead use:
 * 1) A thumbnail view for each vote.  This thumbnail view will be some small size,
 * maybe 320x240 per option to support fitting multiple on one page.  It should have a basic image
 * and text description w/ relatively consistent resolution.
 * 2) A custom page for each option.  Custom content.  Make a Lift template for it.  Resolution is
 * anything.
 * 3) An AJAX form for voting.  The form should persist across pages, allow vote adjustment
 * and prevent duplicate voting (which is almost as hard as allowing vote adjustment).
 * 4) 
 */
object abtree extends DispatchSnippet {

	//we want this to be immutable so that it never locks threads.  ever
	def options = List("ShoutWhisper", "RampageJS", "SchemeTouch", "Irrationale", "Wherefore@Thou")
	
	def submitCode =  """$("#pollform").submit(function() {"
		$("#items").val($("#sortable").sortable('toArray'))
		})"""
		
	def activateCode = """$(function() {
		$( "#sortable" ).sortable();
		$( "#sortable" ).disableSelection();
		});"""

	
	def dispatch = {
		case "sortList" => sortList _
		case "sortlist" => sortList _
		case _ => render()
	}
	
	def sortList(in: NodeSeq) = {
		//TODO: add database check
		bind("poll", in, "make" -> Random.shuffle(options).map(name => "<li id=" + name + ">" + name + "</li>").mkString("\n"))
	}
	
	def getJavascript(in: NodeSeq) = {
		Unparsed("<script>\n" + submitCode + "\n" + activateCode + "\n</script>")
	}
	
	def render()= {
		var items = ""
		
		/**
		 * Function that performs server-side processing.  We might actually be able to check
		 * to see if the email was bounced and place a message saying that something went
		 * wrong with the sending.
		 * 
		 * Code is highly sequential due to staging concerns - we don't want the system
		 * to progress until it's sure that the last operation has completed successfully.
		 */
		def processServer(){
			
		}
		
		
		"name=email" #> (SHtml.text(items, items = _)
				++ SHtml.hidden(processServer))
	}
}