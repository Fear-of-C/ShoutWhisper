/*package fearofsoftware.fourthwall.model

import net.liftweb.mapper._
import com.restfb.DefaultFacebookClient

object FBAccount extends FBAccount with LongKeyedMetaMapper[FBAccount]{
	
}

class FBAccount extends LongKeyedMapper[FBAccount] with IdPK with OneToMany[Long, FBAccount] with ManyToMany {
  def getSingleton = FBAccount
  
  //TODO: find a way to make this forceably unique
  object fbId extends MappedText(this)
  object users extends MappedManyToMany(UserFBAccount, UserFBAccount.fb, UserFBAccount.user, User)
  object accessToken extends MappedText(this)
  

  
  def findById(id : String) : FBAccount = {
  	val allAccounts = FBAccount.findAll(By(FBAccount.fbId, id))
  	if(allAccounts.isEmpty) null else allAccounts.first
  }
}

class UserFBAccount extends Mapper[UserFBAccount]{
	def getSingleton = UserFBAccount
	
	object fb extends LongMappedMapper(this, FBAccount)
	object user extends LongMappedMapper(this, User)
}

object UserFBAccount extends UserFBAccount with MetaMapper[UserFBAccount]*/