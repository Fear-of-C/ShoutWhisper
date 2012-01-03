package fearofsoftware.mainsite.snippet


import net.liftweb._
import http._
import scala.xml._
import common._
import util.Helpers._
import js._
import js.JsCmds._
import JE._
import net.liftweb.util.Mailer._

import fearofsoftware.mainsite.model.Address
import fearofsoftware.mainsite.model.AddressConfirmation
import net.liftweb.mapper.By

import java.security.MessageDigest
import java.util.Random

object emailmanagement extends DispatchSnippet{

	def dispatch = {
		case "confirm" => confirm _
		case "unsubscribe" => unsubscribe _
		case _ => render()
	}
	
	def confirm(html : NodeSeq) = {
		var emailText = ""
		val hashText = S.param("confirm") openOr null
		if(hashText != null){
			emailText = "We couldn't find your email address needing confirmation.  Either " +
				"you've already confirmed, or you are no longer signed up."
			val addrConfirmed : AddressConfirmation = 
				AddressConfirmation.find(By(AddressConfirmation.digest, hashText)) openOr null
			if((addrConfirmed != null) && addrConfirmed.delete_!){
				emailText = "Welcome to the Fear of Software mailing list."
			}
		}
		bind("message", html, "text" -> emailText)
	}
	
	def unsubscribe(html : NodeSeq) = {
		var emailText = ""
		val email = S.param("unsubscribe") openOr null
		if(email != null){
			emailText = "Your email did not appear to be in our records to begin with."
			val addrConfirmed : AddressConfirmation = 
				AddressConfirmation.find(By(AddressConfirmation.emailText, email)) openOr null
			if(addrConfirmed != null){
				addrConfirmed.delete_!
			}
			val addr : Address = Address.find(By(Address.email, email)) openOr null
			if((addr != null) && addr.delete_!){
				emailText = "Your email address is removed from our records"
			}
			
		}
		bind("message", html, "text" -> emailText)
	}
	
	def render()= {
		//for the closure - but could the closure be memory leaking?
		var email = ""
		
		val whence = S.referer openOr "/"
		
		/**
		 * Function that performs server-side processing.  We might actually be able to check
		 * to see if the email was bounced and place a message saying that something went
		 * wrong with the sending.
		 * 
		 * Code is highly sequential due to staging concerns - we don't want the system
		 * to progress until it's sure that the last operation has completed successfully.
		 */
		def processServer(){
			S.clearCurrentNotices
			if(email.matches(""".*?@.*?\..*""")){
				//now we put in code for checking against the database and such
				val preexisting : Box[Address] = Address.find(By(Address.email, email))
				if(!preexisting.isEmpty){
					S.error("Your email address is already in our database.  Please check your spam filter.");
					return
				}
				
				val digest = MessageDigest.getInstance("SHA-256")
				digest.update((email + (new Random()).nextGaussian()).getBytes("UTF-8"))
				val digested = digest.digest
				digest.reset()
				val digestBuilder = new StringBuilder();
				for (i <- 0 until digested.length){
					//sequential for loop, because order matters
					val text = Integer.toHexString(0xFF & digested(i))
					digestBuilder.append(
							text.length() match{
								case 2 => text
								case 1 => "0" + text
							}
					)
				}
				val digestText = digestBuilder.toString
				val link = "http://" + S.hostName + "/manageemail.html?confirm=" + digestText
				val emailText = "You have signed up for the Fear of Software beta mailing list.  To confirm your subscription, visit: \n\n" + link + "\n\n" +
					"If you did not intend to receive this email, ignore it and we will delete your address from our records in several days." +
					"If you would like to unsubscribe, please visit http://" + S.hostName + "/manageemail.html?unsubscribe=" + email + " .";
				//1st validation step: see if the address is already databased
				//2nd: send the confirmation email
				sendMail(From("subscriptions1@fearofsoftware.com"),
				         Subject("Confirm Subscription"),
				         To(email),
				         PlainMailBodyType(emailText));
				//3rd: assuming that didn't blow up, enter it into the database	
				val addr : Address = Address.create
				addr.email(email)
				val hash : AddressConfirmation = AddressConfirmation.create
				hash.digest(digestText)
				hash.emailText(email)
				if(!(hash.save && addr.save)){
					S.error("There was a problem recording your email address.  Sorry.  Check back later.")
				}
				//4th: finish the job by confirming that it was sent
				S.notice("You should receive a confirmation email shortly.")
				//SHtml.ajaxInvoke(hideForm)
				//TOOD: actually use the form hiding
			}
			else{
				S.error("Invalid email address")
			}
		}
		
		/**
		 * Function sent back to client.  This waits a little bit and then hides the
		 * email form.  This should only activate upon successful use.
		 * 
		 * A semi-nice side-effect is that it becomes harder for bots and/or malicious
		 * users to spam the page.  Not much harder, but a little bit.
		 * 
		 * TODO: figure out how to close the AJAX connection
		 */
		def hideForm(): JsCmd ={
			Thread.sleep(400);
			JsHideId("signupform")
		}
				
		
		"name=email" #> (SHtml.text(email, email = _)
				++ SHtml.hidden(processServer))
	}
}