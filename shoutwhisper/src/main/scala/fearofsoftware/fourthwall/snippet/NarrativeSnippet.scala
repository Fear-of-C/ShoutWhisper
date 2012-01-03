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

object NarrativeSnippet extends DispatchSnippet{
	
	
	def appId = S.hostName match {
		case "localhost" => "159433300786101"
		case "fearofsoftware.com" => "107573816016358"
	}
	
	def twitterRedirect = {
		"http://" + S.hostName + "/"
	}
	
	def twitterConsumerKey = "dSEQ8e1YomaSDDvcbQSIuA"
    def twitterConsumerSecret = "MZ1pzVT3uEtQUgTyTgLD3eEQLCAb2555ZChkKa4h7M4"
	
    def twitterURL = "https://www.facebook.com/dialog/oauth?" +
     "client_id=" + appId + "&redirect_uri=" + twitterRedirect +
     "&scope=" + "read_stream"
     
     def createService : OAuthService  = new ServiceBuilder()
                           .provider(classOf[TwitterApi])
                           .apiKey(twitterConsumerKey)
                           .apiSecret(twitterConsumerSecret)
                           .callback(twitterRedirect)
                           .build()
                           
	
    def services : scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
                           
	def dispatch() = {
		case "twitterTest" => twitterLogin
		case _ => testBox
	}
	
	def testBox(html : NodeSeq) : NodeSeq = <p>Default Test</p>
		/*<p>{SHtml.ajaxButton("Testing this button", () => {
				JsCmds.Alert("this button works")
						})}</p>*/
		
		//("id=agentDisplay" #> <div id="agentDisplay">{renderAgents}</div>)(html)
	
	def twitterLogin(html : NodeSeq) : NodeSeq = {
		
		val vString = S.param("oauth_verifier") openOr null
		
		val (service, requestToken) : (OAuthService, Token) = twitterTestService.is match{
			case Full(s) => s
			case _ => {
				println("adding narrativeTest")
				val s = createService
				val t = s.getRequestToken
				twitterTestService.set(Box[(OAuthService, Token)](s, t))
				services += (("narrativeTest", (s, t)))
				(s, t)
			}
		}
		
		if(vString == null){
			val authURL : String = service.getAuthorizationUrl(requestToken)
			<span id="twitterlogin"><a href={authURL}>Login</a> to Twitter.</span>
		}
		else{
			try{
				val v : Verifier  = new Verifier(vString)
				val accessToken : Token = service.getAccessToken(requestToken, v)
				val idReq = new OAuthRequest(Verb.GET, "https://api.twitter.com/1/account/verify_credentials.json")
				service.signRequest(accessToken, idReq)
				val userInfo : JSONObject = JSONValue.parse(idReq.send.getBody).asInstanceOf[JSONObject]
				val twitterId = userInfo.get("id").toString //May need to use JSONParser or JSONValue class to parse this into a proper string
				
				if (twitterId == null)
					println("twitterId == null; this could be a problem")
					
				twitterTestService.set(Empty)
				
				cometTestClient.is match{
					case Full(cc) => cc.externalInit
					case _ => println("not full")//do nothing
				}
				
			
			
			<p>Twitter Logged In</p>
			
			
				//<script type="text/javascript">window.location = "{fbPage}";</script>
			
			}
			catch{
				case o: org.scribe.exceptions.OAuthException => 
				o.printStackTrace
				<span>Error logging into Twitter, please try again</span>
				
				case _ => println("some other error")
				<span>Error...</span>

			}
			
		}
		
	}
	
	def parseTwitter(entry : JSONObject) : Map[String, Any] = {
		parseJSONObject(entry).asInstanceOf[Map[String, Any]]
	}
	
	def formatTwitter(traits : Map[String, Any]) : String = {
		var newString = ""
		traits.foreach{case (k, v) =>
			k match{
				case "text" => newString += v + "\n"
				case _ => newString += ""
			}
		}
		newString
	}
	
	private def parseJSONObject(o : JSONObject) : Map[String, Any] = {
		var traits = Map[String, Any]()
			val entries = o.keySet.toArray.toList
		
			entries.foreach{ t =>
				val tVal = o.get(t)
				if (tVal.isInstanceOf[JSONObject])
					traits += ((t.toString, DiscourseObject.make(parseJSONObject(tVal.asInstanceOf[JSONObject]))))
				else if (tVal.isInstanceOf[JSONArray])
					traits += ((t.toString, DiscourseObject.make(parseJSONArray(tVal.asInstanceOf[JSONArray]))))
				else{
					traits += ((t.toString, tVal))
				}
			}
			
			traits
	}
	
	private def parseJSONArray(o : JSONArray) : Map[String, Any] = {
		var elts = Map[String, Any]()
			val l = o.toArray
			Range(0, l.size).foreach{i =>
				if (l(i).isInstanceOf[JSONObject])
					elts += ((i + "", parseJSONObject(l(i).asInstanceOf[JSONObject])))
				else
					elts += ((i + "", l(i)))
			}

			elts
	}
	
	def narrativeTest : NodeSeq = {
		
		println("this is running")
		
		if (!NarrativeSnippet.services.contains("narrativeTest")){
		
			println("prematurely returning; details: " + NarrativeSnippet.services.toString)
			return <p></p>
		}
		else
			println("token: " + NarrativeSnippet.services.get("narrativeTest"))
		
			
		val streamRequest : OAuthRequest = new OAuthRequest(Verb.GET, "https://stream.twitter.com/1/statuses/sample.json")
		val service : OAuthService = NarrativeSnippet.services.get("narrativeTest").get.asInstanceOf[(OAuthService, Token)]._1.asInstanceOf[OAuthService]
		val accessToken : Token = NarrativeSnippet.services.get("narrativeTest").get.asInstanceOf[(OAuthService, Token)]._2.asInstanceOf[Token]
		service.signRequest(accessToken, streamRequest)
			val streamResponse : Response = streamRequest.send()
			
			val streamReader = new java.io.BufferedReader(new java.io.InputStreamReader(streamResponse.getStream))
			
			var streamString = ""
			
			var line = ""
				
			def getLine = { 
				try{
				line = streamReader.readLine
				if (line != null) true	else false}
				catch{
					case _ => false
				}
			}
				
			while (getLine == true)
			{
				if (line != null){
					println("line: " + line)
					//streamString += NarrativeSnippet.formatTwitter(NarrativeSnippet.parseTwitter(JSONValue.parse(line).asInstanceOf[JSONObject])) + "\n"
			}}
			println("did we get this far?")
			println("stream: " + streamString)
			
			
			
			
		<p>Unparsed(streamString)</p>
			
		
	}

}

object twitterTestService extends SessionVar[Box[(OAuthService, Token)]](Empty)
object cometTestClient extends SessionVar[Box[CometClient]](Empty)

case class RenderOrder