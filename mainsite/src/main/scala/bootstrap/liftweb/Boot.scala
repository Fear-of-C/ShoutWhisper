package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.actor._
import provider._
import Helpers._
import _root_.net.liftweb.sitemap._
import Loc._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import javax.mail._

import fearofsoftware.mainsite.snippet._
import fearofsoftware.mainsite.lib.SessionKiller


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
	def boot {
		if (!DB.jndiJdbcConnAvailable_?) {
			val vendor = 
				new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
						Props.get("db.url") openOr "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
						Props.get("db.user"), Props.get("db.password"))

			LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

			DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
		}

		// where to search snippet
		LiftRules.addToPackages("fearofsoftware.mainsite")
		//Schemifier.schemify(true, Schemifier.infoF _, User)

		//configure email
		def configMailer(host: String, user: String, password: String) {
			// Enable TLS support
			System.setProperty("mail.smtp.starttls.enable","true");
			// Set the host name
			System.setProperty("mail.smtp.host", host) // Enable authentication
			System.setProperty("mail.smtp.auth", "true") // Provide a means for authentication. Pass it a Can, which can either be Full or Empty
			Mailer.authenticator = Full(new Authenticator {
				override def getPasswordAuthentication = new PasswordAuthentication(user, password)
			})
		}

		configMailer("smtp.webfaction.com", "fearsoftware_subscribe", "((em1))")

		LiftRules.early.append(makeUtf8)

		LiftRules.snippetDispatch.append(Map("utilities" -> utilities))

		LiftRules.snippetDispatch.append(Map("emailmanagement" -> emailmanagement))

		LiftRules.dispatch.append {
				case r @ Req("machination" :: "index" :: Nil, _, GetRequest) => 
					() =>Full(PermRedirectResponse("/schemetouch/",r)) 
				case r @ Req("demos" :: "index" :: Nil, _, GetRequest) => 
					() =>Full(PermRedirectResponse("/",r)) 
				case r @ Req("videos" :: "intro" :: "index" :: Nil, _, GetRequest) => 
					() =>Full(PermRedirectResponse("/about/",r)) 
				case r @ Req("4thwall" :: "index" :: Nil, _, GetRequest) => 
					() =>Full(PermRedirectResponse("/",r))
					
				case r @ Req("machination" :: Nil, _, GetRequest) => 
					() =>Full(PermRedirectResponse("/schemetouch/",r)) 
				case r @ Req("demos" :: Nil, _, GetRequest) => 
					() =>Full(PermRedirectResponse("/",r)) 
				case r @ Req("videos" :: "intro" :: Nil, _, GetRequest) => 
					() =>Full(PermRedirectResponse("/about/",r)) 
				case r @ Req("4thwall" :: Nil, _, GetRequest) => 
					() =>Full(PermRedirectResponse("/",r))
				
				case r @ Req("bart" :: Nil, _, GetRequest) =>
					() => Full(PermRedirectResponse("/bart/",r ))
				case r @ Req("schemetouch" :: Nil, _, GetRequest) =>
					() => Full(PermRedirectResponse("/schemetouch/",r ))
				case r @ Req("billyandthebear" :: Nil, _, GetRequest) =>
					() => Full(PermRedirectResponse("/billyandthebear/", r))
				case r @ Req("about" :: Nil, _, GetRequest) =>
					() => Full(PermRedirectResponse("/about/", r))
				case r @ Req("manageemail" :: "index" :: Nil, _, GetRequest) =>
					() => Full(PermRedirectResponse("/manageemail", r))
					
				case r @ Req("index.php" :: Nil, _, GetRequest) =>
					() => Full(PermRedirectResponse("/", r))
		}
		/*
		LiftRules.rewrite.prepend(NamedPF("TrailingSlash")){
			
		})
		LiftRules.rewrite.prepend(NamedPF("DeletedSections") {
		  case RewriteRequest(
		      ParsePath("machination" :: "index" :: Nil, "", true, true), _, _) => 
		    RewriteResponse(
		      "schemetouch" :: "index" :: Nil)
		  case RewriteRequest(
		      ParsePath("demos" :: "index" :: Nil, "", true, true), _, _) => 
		    RewriteResponse(
		      "index" :: Nil)
		  case RewriteRequest(
		      ParsePath("videos" :: "intro" :: "index" :: Nil, "", true, true), _, _) => 
		    RewriteResponse(
		      "about" :: "index" :: Nil)
		  case RewriteRequest(
		      ParsePath("4thwall" :: "index" :: Nil, "", true, true), _, _) => 
		    RewriteResponse(
		      "index" :: Nil)
		})
		*/


		//use html5
		LiftRules.htmlProperties.default.set((r: Req) =>
		new Html5Properties(r.userAgent))

		//don't strip out comments in html - the boilerplate needs them
		LiftRules.stripComments.default.set(() => false)

		LiftRules.useXhtmlMimeType = false

		// Build SiteMap
		def sitemap() = SiteMap(
				Menu.i("Home") / "index",
				Menu.i("Bart") / "bart" / "index",
				Menu.i("Scheme Touch") / "schemetouch" / "index",
				Menu.i("Billy and the Bear") / "billyandthebear" / "index" >> Hidden,
				Menu.i("About") / "about" / "index",
				Menu.i("Email") / "manageemail"
				//Menu.i("Test") / "test"

				//Menu.i("static") / "static" / ** >> Hidden
		)
		

		/*
		 * For now, the mailing list signup needs some state.  We should find out how to kill
		 * it once a signup occurs, and furthermore, possibly port it over to the "old fashioned"
		 * way that does not require state.
		 */
		LiftRules.statelessTest.append {
		case "about" :: _ => true
		case "billyandthebear" :: _ => true
		}

		LiftRules.setSiteMapFunc(() => sitemap())

		/*
		 * Show the spinny image when an Ajax call starts
		 */
		LiftRules.ajaxStart =
			Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

		/*
		 * Make the spinny image go away when it ends
		 */
		LiftRules.ajaxEnd =
			Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)
	
		SessionMaster.sessionCheckFuncs = SessionMaster.sessionCheckFuncs ::: List(SessionKiller)

		SessionMaster.sessionWatchers = SessionAdjuster ::
			SessionMaster.sessionWatchers
	}

	/**
	 * Force the request to be UTF-8
	 */
	private def makeUtf8(req: HTTPRequest) {
		req.setCharacterEncoding("UTF-8")
	}
}

object SessionAdjuster extends LiftActor{
	private var lastTime = millis
	
	private def PANIC_PERCENT = 35L

	private def cyclePeriod = 1 minute
	
	protected def messageHandler = {
		case SessionWatcherInfo(sessions) =>
		if ((millis - cyclePeriod) > lastTime) {
			lastTime = millis
			val rt = Runtime.getRuntime
			//this is interesting - the example code actually uses a hard-coded gc call here
			rt.gc
			val percent = (rt.freeMemory * 100L) / rt.totalMemory
	
			// get more aggressive about purging if we're  low on memory
			if (percent < PANIC_PERCENT) {
				SessionKiller.killWhen /= 2L
				if (SessionKiller.killWhen < 5000L)
					SessionKiller.killWhen = 5000L
				SessionKiller.killCnt *= 2
			} else {
				SessionKiller.killWhen *= 2L
				if (SessionKiller.killWhen > SessionKiller.defaultKillWhen)
					SessionKiller.killWhen = SessionKiller.defaultKillWhen
				val newKillCnt = SessionKiller.killCnt / 2
				if (newKillCnt > 0) SessionKiller.killCnt = newKillCnt
			}
		}
	}
}
