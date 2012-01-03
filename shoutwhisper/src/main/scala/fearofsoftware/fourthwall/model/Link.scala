package fearofsoftware.fourthwall.model


import net.liftweb.mapper._

class LinkTraitImpl extends LongKeyedMapper[LinkTraitImpl] with OneToMany[Long, LinkTraitImpl] with ManyToMany with IdPK with TransactSaveable{

	def getSingleton = LinkTraitImpl
	
	object tr extends LongMappedMapper(this, Trait){
		 override def dbIndexed_? = true
	}
	
	object value extends LongMappedMapper(this, DiscourseObject)
}

object LinkTraitImpl extends LinkTraitImpl with LongKeyedMetaMapper[LinkTraitImpl]{
	
}