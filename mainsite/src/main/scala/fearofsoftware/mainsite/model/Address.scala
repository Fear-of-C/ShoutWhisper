package fearofsoftware.mainsite.model

import net.liftweb.mapper._

object Address extends Address with LongKeyedMetaMapper[Address]{
	  override def dbTableName = "addresses" // define the DB table name

}

class Address extends LongKeyedMapper[Address]{
	def getSingleton = Address
	
	def primaryKeyField = id
	
	object id extends MappedLongIndex(this)
	object email extends MappedString(this, 250){
		override def dbIndexed_? = true
	}
}

