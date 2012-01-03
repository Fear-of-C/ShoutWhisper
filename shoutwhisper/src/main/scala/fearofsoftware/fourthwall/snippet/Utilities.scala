package fearofsoftware.fourthwall.snippet

import scala.xml._

import net.liftweb._
  import common._
  import http._
    import LiftRules._
    
import util.Helpers._

object Utilities extends DispatchSnippet{
	
	
	def ieDumpText = """<!--[if lt IE 7 ]> <html lang="en" class="no-js ie6"> <![endif]-->
		  <!--[if IE 7 ]>    <div lang="en" class="no-js ie7"> <![endif]-->
		  <!--[if IE 8 ]>    <div lang="en" class="no-js ie8"> <![endif]-->
		  <!--[if IE 9 ]>    <div lang="en" class="no-js ie9"> <![endif]-->
		  <!--[if (gt IE 9)|!(IE)]><!--> <div lang="en" class="no-js"> <!--<![endif]-->"""
	
	def ieCloseText = "</div>"
		
	def analytics = """<script>
		var _gaq=[['_setAccount','UA-21951280-1'],['_trackPageview']];
		(function(d,t){var g=d.createElement(t),s=d.getElementsByTagName(t)[0];g.async=1;
		g.src=('https:'==location.protocol?'//ssl':'//www')+'.google-analytics.com/ga.js';
		s.parentNode.insertBefore(g,s)}(document,'script'));
		  </script>"""
	def blankAnalytics = """<script>
		var _gaq=[['_setAccount','XX'],['_trackPageview']];
		//script redacted
		  </script>"""
		
	def videoString = """<!-- Begin VideoJS -->
			  <div class="video-js-box">
			    <!-- Using the Video for Everybody Embed Code http://camendesign.com/code/video_for_everybody -->
			    
			    <video class="video-js" width="(width)" height="(height)" controls="controls" preload="preload" poster="(video).png">
			      <source src="(video).mp4" type='video/mp4; codecs="avc1.42E01E, mp4a.40.2"' />
			      <source src="(video).webm" type='video/webm; codecs="vp8, vorbis"' />
			      <source src="(video).ogv" type='video/ogg; codecs="theora, vorbis"' />
			      <!-- Flash Fallback. Use any flash video player here. Make sure to keep the vjs-flash-fallback class. -->
			      <object class="vjs-flash-fallback" width="480" height="320" type="application/x-shockwave-flash"
			        data="http://releases.flowplayer.org/swf/flowplayer-3.2.1.swf">
			        <param name="movie" value="http://releases.flowplayer.org/swf/flowplayer-3.2.1.swf" />
			        <param name="allowfullscreen" value="true" />
			        <param name="flashvars" value='config={"playlist":["(videourl).png", {"url": "(videourl).mp4","autoPlay":false,"autoBuffering":true}]}' />
			        <!-- Image Fallback. Typically the same as the poster image. -->
			        <img src="(video).png" width="(width)" height="(height)" alt="Poster Image"
			          title="No video playback capabilities." />
			      </object>
			    </video>
			  </div>
		  <!-- End VideoJS -->"""
		
	def jQueryBackupString = """<script>window.jQuery || document.write('<script src="js/libs/jquery-1.6.2.min.js"><\/script>')</script>"""

	def dispatch() = {
		case "mode" => mode _
		case "analyticsscript" => analyticsScript _
		case "analyticsScript" => analyticsScript _
		case "jQueryBackup" => jQueryBackup _
	    case "contenthost" => contenthost _
	    case "contentHost" => contenthost _
	    case "scriptHost" => scripthost _
	    case "scripthost" => scripthost _
	    case "ieConditionalComment" => ieConditionalComment _
	    case "ieconditionalcomment" => ieConditionalComment _
	    case "iedump" => ieDump _
	    case "ieDump" => ieDump _
	    case "iedivclose" => ieDivClose _
	    case "ieDivclose" => ieDivClose _
	    case "videotag" => videoTag _
	    case "videoTag" => videoTag _
		}

	
	def ieDivClose(xhtml : NodeSeq) = {
	  Unparsed(ieCloseText)
  }
	
	  def ieDump(xhtml : NodeSeq) = {
	  Unparsed(ieDumpText)
  }

	  
	  	
	def videoTag(xhtml : NodeSeq) = {
		val videoName = "/static-content/" + (S.attr("name") openOr "video")
		val videoWidth = S.attr("width") openOr "width"
		val videoHeight = S.attr("height") openOr "height"
		val videoUrl = "http://" + S.hostName + videoName
		Unparsed(
				videoString.replace("(video)", videoName).replace("(width)", videoWidth).replace("(height)", videoHeight).replace("(videourl)", videoUrl)
		)
		
	}
	
  /**
   * Insert a conditional comment. Ensures that the comment (a) doesn't get
   * stripped in production mode and (b) can contain XML that is pre-processed
   * by, for example, with-resource-id.
   */
  def ieConditionalComment(xhtml:NodeSeq) = {
    val ieVersion =
      (for (version <- S.attr("version")) yield version) openOr "IE"
    Unparsed("<!--[if " + ieVersion + "]>") ++ xhtml ++ Unparsed("<![endif]-->")
  }
	def mode(xhtml:NodeSeq) = {
		val result =
			for {
				targetMode <- S.attr("is") ?~ "is attribute for target mode was not specified"
					mode <- Box.legacyNullTest(System.getProperty("run.mode")) ?~ "target mode not found"
			} yield {
				if (targetMode == mode) {
					xhtml
				} else {
					NodeSeq.Empty
				}
			}

			result match {
			case Full(data) => data
			case Empty => NodeSeq.Empty
			case Failure(message, _, _) => <div class="error">{message}</div>
			}
	}

	def analyticsScript(xhtml : NodeSeq) = {
		S.hostName match{
		case "localhost" => Unparsed(blankAnalytics)
		case _ => Unparsed(analytics)
		}
	}
	
	def jQueryBackup(html : NodeSeq) = {
		Unparsed(jQueryBackupString)
	}
	
	def contenthost(xhtml: NodeSeq) = {
		
		var hostName = S.hostName
		hostName = "http://" + hostName + "/static-content"
		
		var bound = xhtml
		
		bound = bind("videosource", bound, FuncAttrBindParam("src", {ns : NodeSeq => Text(hostName + ns)}, "src"))
		bound = bind("videocss", bound, FuncAttrBindParam("href", {ns : NodeSeq => Text(hostName + ns)}, "href"))
		bound = bind("postersource", bound, FuncAttrBindParam("poster", {ns : NodeSeq => Text(hostName + ns)}, "poster"))
		bind("flashvideo", bound, FuncAttrBindParam("value", 
				{ns : NodeSeq => Unparsed(if(ns.contains("static")) ns.toString() else {
				"config={\"playlist\":[\"/" + hostName + ns +".png\", {\"url\": \"/" + hostName + ns + ".mp4\"," +
				"\"autoPlay\":false,\"autoBuffering\":true}]}"})},
				"value"))
	}
	
	/**
	 * Binding alterations for GWT and other large javascript sources.
	 * 
	 * For some reason, it would appear that externally linking scripts is a poor strategy.
	 * Do we have any other way of doing this that works on localhost?
	 * This should actually be fixable on the main server, as it will come from the same domain.
	 */
	def scripthost(xhtml: NodeSeq) = {
		
		var hostName = S.hostName
		hostName = "http://" + hostName + "/static2"
		
		var bound = xhtml
		
		bound = bind("scriptsource", bound, FuncAttrBindParam("src", {ns : NodeSeq => Text(hostName + ns)}, "src"))
		bind("scriptcss", bound, FuncAttrBindParam("href", {ns : NodeSeq => Text(hostName + ns)}, "href"))
	}
}