/*package fearofsoftware.fourthwall.snippet

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


/**
* This will take care of everything we need to do client-side dynamically before the user
* is logged in (and a CometActor has any purpose).
* 
* Logging into this kind of service is complicated, because there are multiple methods for it.
* We should have:
* 1) A way to log in from each kind of service that one uses.
* 2) A way to forbid a certain account from being used for general login, in case it's communal.
* 3) A way that an account is automatically created when one attempts to login with a service.
* 4) A way to create new accounts on services for monitoring w/out hurting the login account.
* 5) A way to delete individual service accounts or everything at once.
* 
* For simplicity, we need 1-click login as far as possible.  This means that we only ask the user
* to login multiple places if their access tokens have expired (we hold access tokens even when
* they're logged out).
*/
object ClientSnippet extends DispatchSnippet{
	
	def fbRedirect = {
		"http://" + S.hostName + "/"
	}
	def twitterRedirect = {
		"http://" + S.hostName + "/"
	}
	def appId = S.hostName match {
		case "localhost" => "159433300786101"
		case "fearofsoftware.com" => "107573816016358"
	}
	def fbURL = "https://www.facebook.com/dialog/oauth?" +
     "client_id=" + appId + "&redirect_uri=" + fbRedirect +
     "&scope=" + "read_stream" +",offline_access"
    def fbPage = {
		"http://" + S.hostName + "/"
	}
    def appSecret = S.hostName match {
    	case "localhost" => "3131b9554e513148e42264b76c686659"
    	case "fearofsoftware.com" => "619d3f596c09a90ed3daafde3969d759"
    }
    def fbURL2 = "https://graph.facebook.com/oauth/access_token?" + 
     "client_id=" + appId + "&redirect_uri=" + fbRedirect +
     "&client_secret=" + appSecret + "&code="
     
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
			
     
			
	
    
    //The nonce is a unique number for each request.  Seeding it with the timestamp
    def getNonce(l: Long) =  new Random(l)
    	
	def dispatch() = {
		case "master" => masterLogin _
		case "facebook" => fbLogin _
		case "twitter" => twitterLogin _
		case "logout" => logout _
		//case "manage" => accountManage _
	}
	
	/**
	 * This is the content for the 1st index page the user will come across.
	 */
	def masterLogin(html : NodeSeq) : NodeSeq = {
		<p>{fbLogin(html)}</p><p>{twitterLogin(html)}</p>
	}
	
	/**
	 * This should return content to put in an AJAX dialog or similar
	 */
	def addRSSFeed(html : NodeSeq) : NodeSeq = {
		<span id="fblogin"><a href={fbURL}>Login</a> to Facebook to start.</span>
	}
	
	def logout(html : NodeSeq) : NodeSeq = {
		User.currentUser match{
			case Full(user) => SHtml.ajaxButton("Logout", () => {
				 User.logUserOut
				 new JE.JsRaw("""window.location = "/";""").cmd
			})
			case _ => <span></span>
		}
	}
	
	
	/**
	 * We may want to automatically log the user into our system as well, if they're not
	 * already in.  We want to avoid making users register separately - so a FB login
	 * should automatically log them in.
	 * 
	 * There is probably also a screen for logging in just with the user account.
	 */
	def fbLogin(html : NodeSeq) : NodeSeq= {
		
		val code = S.param("code") openOr null
		if(code == null){
			User.currentUser match{
				case Full(user) => {
					if (!user.facebooks.isEmpty)
						<span>You are logged in with Facebook.</span>
					else
						<span id="fblogin"><a href={fbURL}>Login</a> to Facebook.</span>
				}
				case _ => <span id="fblogin"><a href={fbURL}>Login</a> to Facebook.</span>
			}
		}else{
			val authURL = new URL(fbURL2 + code)
			val tokenContent : sun.net.www.content.text.PlainTextInputStream  = authURL.openConnection().getContent().asInstanceOf[sun.net.www.content.text.PlainTextInputStream]
			val tokenReader = new java.io.InputStreamReader(tokenContent)
			
			
			val tokenBuffer = new Array[Char](1000)
			
			val tokenLength = tokenReader.read(tokenBuffer, 0, 1000)
			
			val tokenArray = new Array[Char](tokenLength)
			
			for (i <- 0 until tokenLength){
				tokenArray(i) = tokenBuffer(i)
			}
			
			val tokenString = new String(tokenArray)
			
			val token = tokenString.split("&")(0).split("=")(1)
			
			val fbClient = new DefaultFacebookClient(token)
			val fbUser = fbClient.fetchObject("me", classOf[com.restfb.types.User])
			val fbId = fbUser.getId
			
			var fbAccount = FBAccount.find(By(FBAccount.fbId, fbId)) openOr null
			val fbUsers : Set[User] = if(fbAccount == null) Set[User]() else fbAccount.users.toSet
			
			def createFBAccount(user : User) = {
				val newFB = FBAccount.create
				newFB.fbId(fbId)
				newFB.accessToken(token)
				newFB.save
				newFB.users += user
				newFB.save
				newFB
			}
			
			/*
			 * If a user is already logged in, then associate this with their account.
			 * 
			 * If a user is not already logged in, attempt to log them in.
			 * 
			 * If a user is not registered, automatically use a "login with Facebook"
			 * type of system.
			 */
			User.currentUser match{
				case Full(user) => {
					//then the user is already logged in - see if they have this id
					if(fbUsers.contains(user)){
						//this is actually redundant and a pathological case
						//we probably do nothing here, maybe log an error
						println("Pathological case")
					}else{
						fbAccount match{
							case null =>{
								//then we have a new facebook to create
								fbAccount = createFBAccount(user)
							}
							case _ =>{
								//then we add the new account to the user's accounts
								//we don't need to ask, since they shouldn't have been here otherwise
								fbAccount.users += user
								//user.facebooks += fbAccount
								fbAccount.save
							}
							user.save
						}							
					}
				}
				case _ => {
					//then we just have Facebook - see if we can find account(s)
					fbUsers.size match{
						case 1 => {
							//then they just have 1 account, so log them in.
							User.logUserIn(fbUsers.first)
						}
						case 0 => {
							//then they don't have an account, so make one for them from scratch
							val user = User.make
							user.firstName(fbUser.getFirstName)
							user.lastName(fbUser.getLastName)
							user.email(fbUser.getEmail)
							//do they have an account with our service?  It's not likely
							if(fbAccount != null){
								fbAccount.users += user
								//user.facebooks += fbAccount
								fbAccount.save
							}else{
								fbAccount = createFBAccount(user)
							}
							user.save
							User.logUserIn(user)
						}
						case _ => {
							//if they have multiple accounts, we need to ask which to login with
							//display a dialog with radio buttons
							val accountUsers : Map[Long, User] = fbUsers.map(user => (user.id.is, user)).toMap
							val accountDates : Map[Long, Date] = fbUsers.map(user => (user.id.is, user.getDiscourse.get[Date]("date created"))).toMap
							var chosenUser : Long = accountUsers.keys.first
							def chooseAccount = {
								//this contains the code for selecting the user and moving on
								val user = accountUsers(chosenUser)
								User.logUserIn(user)
							}
							//TODO: here we return a bunch of AJAX form madness
						}
					}
				}
			}
			
			Director.addFB(fbId, fbClient)
			
			cometClient.is match{
				case Full(cc) => cc.externalInit
				case _ => //do nothing
			}
			
			<script type="text/javascript">window.location = "{fbPage}";</script>

		}
	}
	
	
	def twitterLogin(html : NodeSeq) : NodeSeq = {
		User.currentUser match{
			case Full(user) => {
				if(!user.twitters.isEmpty){
					return <span>You are logged in with Twitter.</span>
				}
			}//do nothing, just continue onward
			case _ => println("continuing onward...")//return <span></span>
		}
		val vString = S.param("oauth_verifier") openOr null
		
		val (service, requestToken) : (OAuthService, Token) = twitterService.is match{
			case Full(s) => s
			case _ => {
				val s = createService
				val t = s.getRequestToken
				twitterService.set(Box[(OAuthService, Token)](s, t))
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
			
			/*
			 * twitterAccount: the account that corresponds to the current ID of the user
			 * twitterUsers: Shout/Whisper accounts that are using that Twitter account
			 */
			var twitterAccount = TwitterAccount.find(By(TwitterAccount.twitterId, twitterId)) openOr null
			val twitterUsers : Set[User] = if(twitterAccount == null) Set[User]() else twitterAccount.users.toSet
			
			
			/*
			 * If a user is already logged in, then associate this with their account.
			 * 
			 * If a user is not already logged in, attempt to log them in.
			 * 
			 * If a user is not registered, automatically use a "login with Facebook"
			 * type of system.
			 */
			User.currentUser match{
				/*
				 * Case: The user is logged in--this could be via facebook or some other service/ID
				 */
				case Full(user) => {
					/*
					 * If the user already has this twitter, then we ought to make sure that
					 * this twitter gets logged into--but that's a bit redundant seeing that that's what we're doing
					 * May not be pathological
					 */
					if(twitterUsers.contains(user)){
						println("Pathological case may be occuring")
					}
					/*
					 * Otherwise, we need to see if there is a twitter account in the database that matches this ID
					*/else{
						twitterAccount match{
							/*
							 * If not, we have to create a new one and put it in the database
							 */
							case null =>{
								val newTwitter = TwitterAccount.create
								newTwitter.twitterId(twitterId)
								newTwitter.accessToken(accessToken.getToken)
								newTwitter.tokenSecret(accessToken.getSecret)
								newTwitter.save
								newTwitter.users += user
								//user.twitters += newTwitter
								newTwitter.save
								twitterAccount = newTwitter
								user.save
							}
							/*
							 * Otherwise, we just add a new user to the current account
							 */
							case _ =>{
								//then we add the new account to the user's accounts
								//we don't need to ask, since they shouldn't have been here otherwise
								twitterAccount.users += user
								//user.twitters += twitterAccount
								twitterAccount.save
								user.save
							}
							
						}							
					}
				}
				/*
				 * Case -- The User is not logged in to any Account/ID
				 */
				case _ => {
					twitterUsers.size match{
						/*
						 * Only one user owns this twitter account, log that user in
						 */
						case 1 => {
							User.logUserIn(twitterUsers.first)
						}
						/*
						 * This twitter account has no user; create a new one
						 */
						case 0 => {
							val user = User.make
							user.firstName(userInfo.get("name").toString)
							user.lastName(userInfo.get("name").toString)
							user.email(userInfo.get("name").toString)
							user.save
							//do they have an account with our service?
							if(twitterAccount != null){
								twitterAccount.users += user
								//user.twitters += twitterAccount
								twitterAccount.save
							}else{
								val newTwitter = TwitterAccount.create
								newTwitter.twitterId(twitterId)
								newTwitter.accessToken(accessToken.getToken)
								newTwitter.tokenSecret(accessToken.getSecret)
								newTwitter.save
								newTwitter.users += user
								//user.twitters += newTwitter
								newTwitter.save
								twitterAccount = newTwitter
							}
							user.save
							User.logUserIn(user)
						}
						/*
						 * This twitter account has multiple users, need to find the right one to log in as
						 * This case will become deprecated since we now want to have a primary identification
						 */
						case _ => {
							//if they have multiple accounts, we need to ask which to login with
							//display a dialog with radio buttons
							val accountUsers : Map[Long, User] = twitterUsers.map(user => (user.id.is, user)).toMap
							val accountDates : Map[Long, Date] = twitterUsers.map(user => (user.id.is, user.getDiscourse.get[Date]("date created"))).toMap
							var chosenUser : Long = accountUsers.keys.first
							def chooseAccount = {
								//this contains the code for selecting the user and moving on
								val user = accountUsers(chosenUser)
								User.logUserIn(user)
							}
							//TODO: here we return a bunch of AJAX form madness
							println("probably shouldn't be getting this case right now")
						}
					}
				}
			}
			twitterService.set(Empty)
			
			cometClient.is match{
				case Full(cc) => cc.externalInit
				case _ => //do nothing
			}
			
			Director.addTwitter(twitterId, (service, accessToken))
			<script type="text/javascript">window.location = "{fbPage}";</script>
			
			}
			catch{
				case o: org.scribe.exceptions.OAuthException => 
				o.printStackTrace
				<span>Error logging into Twitter, please try again</span>
				
				case _ => println("some other error")
				<span>Error...</span>

			}
			
		}
			
			/*Some random notes about how the code works for streaming and stuff
			 * 
			 * 
			 * val streamRequest : OAuthRequest = new OAuthRequest(Verb.GET, "https://stream.twitter.com/1/statuses/sample.json")
			service.signRequest(accessToken, streamRequest)
			val streamResponse : Response = streamRequest.send()
			
			val streamReader = new java.io.BufferedReader(new java.io.InputStreamReader(streamResponse.getStream))
			
			Unparsed(streamReader.readLine) */
			
			/*
			
			Unparsed("Response " + pollResponse.getBody()) */
			
			
			//Director.addTwitter(twitterId, twitterClient)
			//<div></div>
			
			
	}
}
object twitterService extends SessionVar[Box[(OAuthService, Token)]](Empty)
object cometClient extends SessionVar[Box[CometClient]](Empty)*/