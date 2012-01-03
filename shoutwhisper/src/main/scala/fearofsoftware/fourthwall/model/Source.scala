package fearofsoftware.fourthwall.model

import net.liftweb.mapper._

/**
 * For now, a database class to hold RSS feeds.
 * 
 * A feed can be public but still have owners (everyone who added it).  If it is private, then it 
 * must have users, or else it is up for garbage collection.
 * 
 * Currently, once someone enters a feed as public, it is totally public.
 */
class RSSSource extends LongKeyedMapper[RSSSource] with IdPK with OneToMany[Long, RSSSource] with ManyToMany {
  def getSingleton = RSSSource
  
  object privacy extends MappedBoolean(this){
  	override def dbIndexed_? = true
  }
  object url extends MappedText(this){
  	override def dbIndexed_? = true
  }
  object users extends MappedManyToMany(UserRSSSource, UserRSSSource.rss, UserRSSSource.user, User)
}

object RSSSource extends RSSSource with LongKeyedMetaMapper[RSSSource]{
	
}

class UserRSSSource extends Mapper[UserRSSSource]{
	def getSingleton = UserRSSSource
	
	object rss extends LongMappedMapper(this, RSSSource)
	object user extends LongMappedMapper(this, User)
}

object UserRSSSource extends UserRSSSource with MetaMapper[UserRSSSource]