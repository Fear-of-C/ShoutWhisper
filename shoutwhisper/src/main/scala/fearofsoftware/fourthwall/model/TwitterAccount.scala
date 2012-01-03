/*package fearofsoftware.fourthwall.model

import net.liftweb.mapper._

class TwitterAccount extends LongKeyedMapper[TwitterAccount] with IdPK with OneToMany[Long, TwitterAccount] with ManyToMany {
  def getSingleton = TwitterAccount
  
  object twitterId extends MappedText(this)
  object users extends MappedManyToMany(UserTwitterAccount, UserTwitterAccount.twitter, UserTwitterAccount.user, User)
  object accessToken extends MappedText(this)
  object tokenSecret extends MappedText(this)
  
  def findById(id : String) : TwitterAccount = {
	  val allAccounts = TwitterAccount.findAll(By(TwitterAccount.twitterId, id))
	  if(allAccounts.isEmpty) null else allAccounts.first
  }
  
}

object TwitterAccount extends TwitterAccount with LongKeyedMetaMapper[TwitterAccount]{
	
}

class UserTwitterAccount extends Mapper[UserTwitterAccount]{
	def getSingleton = UserTwitterAccount
	
	object twitter extends LongMappedMapper(this, TwitterAccount)
	object user extends LongMappedMapper(this, User)
}

object UserTwitterAccount extends UserTwitterAccount with MetaMapper[UserTwitterAccount]*/