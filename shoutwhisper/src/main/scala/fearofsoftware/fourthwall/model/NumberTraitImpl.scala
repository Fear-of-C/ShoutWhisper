package fearofsoftware.fourthwall.model

import net.liftweb.mapper._

object NumberTraitImpl extends NumberTraitImpl with LongKeyedMetaMapper[NumberTraitImpl]{
	
}

class NumberTraitImpl extends LongKeyedMapper[NumberTraitImpl] with ManyToMany with IdPK with TransactSaveable{

	def getSingleton = NumberTraitImpl
	
	object value extends MappedLong(this){
		 override def dbIndexed_? = true
	}
	
	object tr extends LongMappedMapper(this, Trait){
		 override def dbIndexed_? = true
	}
}

