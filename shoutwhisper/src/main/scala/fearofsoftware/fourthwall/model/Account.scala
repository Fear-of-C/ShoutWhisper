package fearofsoftware.fourthwall.model

import net.liftweb.mapper._
import com.restfb.DefaultFacebookClient

object Account extends Account with LongKeyedMetaMapper[Account]{
	
}

class Account extends LongKeyedMapper[Account] with IdPK with OneToMany[Long, Account] with ManyToMany {
  def getSingleton = Account
  
  //TODO: find a way to make this forceably unique
  object accountId extends MappedText(this)
  object accountType extends MappedText(this)
  
  /*
   * TODO: Make sure that this is written correctly
   */
  object users extends MappedManyToMany(UserAccount, UserAccount.account, UserAccount.user, User)
  
  object primaryUser extends LongMappedMapper(this, User)
  
  object authentication extends MappedText(this)
  
  
  def findById(id : String) : Account = {
  	val allAccounts = Account.findAll(By(Account.accountId, id))
  	if(allAccounts.isEmpty) null else allAccounts.first
  }
}

class UserAccount extends Mapper[UserAccount]{
	def getSingleton = UserAccount
	
	object account extends LongMappedMapper(this, Account)
	object user extends LongMappedMapper(this, User)
}

object UserAccount extends UserAccount with MetaMapper[UserAccount]