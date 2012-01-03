package bootstrap.liftweb

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import fearofsoftware.fourthwall.snippet._
import fearofsoftware.fourthwall.model.User
import net.liftweb.mapper.Schemifier
import fearofsoftware.fourthwall.model._
import net.liftweb.mapper.DB
import net.liftweb.db.StandardDBVendor
import net.liftweb.db.DefaultConnectionIdentifier
import net.liftweb.mapper.Mapper
import fearofsoftware.fourthwall.lib.Director
import org.scribe._
import org.scribe.oauth._
import org.scribe.model._
import org.scribe.builder._
import org.scribe.builder.api._
import net.liftweb.http.provider.HTTPRequest
import java.util.logging.Logger


/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {

  	//TODO: figure out what RestFB uses and fix that too.
  	//Logger.getLogger(Logger.GLOBAL_LOGGER_NAME).setLevel(java.util.logging.Level.SEVERE)
  	
    if (!DB.jndiJdbcConnAvailable_?) {
		val vendor = 
			new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
					Props.get("db.url") openOr "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
					Props.get("db.user"), Props.get("db.password"))

		LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

		DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
	}
    
    Props.mode match {
	  case Props.RunModes.Development => {
	  	Schemifier.destroyTables_!!(Schemifier.infoF _,
	  		User, Account, UserAccount,
    		DiscourseObject, DateTraitImpl, Relation, RelationTraitImpl, TextTraitImpl, Trait, TraitName, Word,
    		WordUse, LinkTraitImpl, NumberTraitImpl, UniversalAgents,
    		RSSSource, UserRSSSource)
	  	}
	  case _ => {}
	}
	
    Schemifier.schemify(true, Schemifier.infoF _,
	  		User, Account, UserAccount,
    		DiscourseObject, DateTraitImpl, Relation, RelationTraitImpl, TextTraitImpl, Trait, TraitName, Word,
    		WordUse, LinkTraitImpl, NumberTraitImpl, UniversalAgents,
    		RSSSource, UserRSSSource)

	
    // where to search snippet
    LiftRules.addToPackages("fearofsoftware.fourthwall")
	
	def initializeSources = {
    	
    	val accountSources = Account.findAll
    	/*
    	 * Note: if there is an error here, it is probably because of a 
    	 * mismatch between source.accountType and the name used among
    	 * the MiniDirector instances in Director
    	 */
    	accountSources.foreach(source => try{Director.addSourceClient(source.accountId.is, source.accountType.is, Director.toSource(source.accountType.is, source.authentication.is))} catch {case _ => })
    	
    	/*val fbSources = FBAccount.findAll
    	val tweetSources = TwitterAccount.findAll*/
    	val rss = RSSSource.findAll
    	/*fbSources.foreach(source => try{Director.addFB(source.fbId.is, source.accessToken.is)} catch {case _ => })
    	tweetSources.foreach(source => try{Director.addTwitter(
    			source.twitterId.is, (ClientSnippet.createService, new Token(source.accessToken.is, source.tokenSecret.is)))} catch {case _ => })*/
    	rss.foreach(source => try{Director.addRSSFeed(source.url.is)} catch {case _ => })
    }
    initializeSources
    
    LiftRules.snippetDispatch.append(Map("utilities" -> Utilities))
    LiftRules.snippetDispatch.append(Map("client" -> LoginSnippet))
    LiftRules.snippetDispatch.append(Map("manage" -> ManagementSnippet))
    LiftRules.snippetDispatch.append(Map("narrative" -> NarrativeSnippet))
    
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
				case r @ Req("about" :: Nil, _, GetRequest) =>
					() => Full(PermRedirectResponse("/about/", r))
					
				case r @ Req("index.php" :: Nil, _, GetRequest) =>
					() => Full(PermRedirectResponse("/", r))
		}
    
    // Build SiteMap
    def sitemap() = SiteMap(
				Menu.i("Home") / "index",
				Menu.i("Bart") / "bart" / "index",
				Menu.i("Scheme Touch") / "schemetouch" / "index",
				Menu.i("About") / "about" / "index",
				Menu.i("Privacy") / "privacy"
				//Menu.i("Test") / "test"

				//Menu.i("static") / "static" / ** >> Hidden
		)
		

	LiftRules.statelessTest.append {
	case "about" :: _ => true
	}

	LiftRules.setSiteMapFunc(() => sitemap())
	
	LiftRules.early.append((req : HTTPRequest) => req.setCharacterEncoding("UTF-8"))
	//use html5
	LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))
	//don't strip out comments in html - the boilerplate needs them
	LiftRules.stripComments.default.set(() => false)

	LiftRules.useXhtmlMimeType = false

  }}

