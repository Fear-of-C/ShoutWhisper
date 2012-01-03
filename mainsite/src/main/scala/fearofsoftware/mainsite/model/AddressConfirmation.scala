package fearofsoftware.mainsite.model

import net.liftweb.mapper._

object AddressConfirmation extends AddressConfirmation with LongKeyedMetaMapper[AddressConfirmation]{
	  override def dbTableName = "confirm" // define the DB table name

}

/**
 * Mostly trivial table used to store email confirmation hashes.  This prevents users
 * from auto-subscribing other users by running confirmations through difficult-to-guess
 * hashes, preventing malicious users from fully auto-subscribing others.
 * 
 * Note that it is impossible to back-search this by address.  Do we care?  Maybe not.
 * If we let people resend confirmation emails, a bot could DOS someone's email with that.
 */
class AddressConfirmation extends LongKeyedMapper[AddressConfirmation]{
	def getSingleton = AddressConfirmation
	
	def primaryKeyField = id
	
	object id extends MappedLongIndex(this)
	object digest extends MappedString(this, 64){
		override def dbIndexed_? = true
	}
	object emailText extends MappedEmail(this, 250){
		override def dbIndexed_? = true
	}
}

