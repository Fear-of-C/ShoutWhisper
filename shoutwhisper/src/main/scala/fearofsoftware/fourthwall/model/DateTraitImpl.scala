package fearofsoftware.fourthwall.model

import net.liftweb.mapper._

object DateTraitImpl extends DateTraitImpl with LongKeyedMetaMapper[DateTraitImpl]{
	
}

class DateTraitImpl extends LongKeyedMapper[DateTraitImpl] with ManyToMany with IdPK with TransactSaveable{

	def getSingleton = DateTraitImpl
	
	object value extends MappedDateTime(this){
		 override def dbIndexed_? = true
	}
	
	object tr extends LongMappedMapper(this, Trait){
		 override def dbIndexed_? = true
	}
}

