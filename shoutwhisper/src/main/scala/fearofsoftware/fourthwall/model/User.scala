package fearofsoftware.fourthwall.model

import net.liftweb.mapper._
import net.liftweb.common.Full

class User extends MegaProtoUser[User]  with OneToMany[Long, User] with ManyToMany {
	def getSingleton = User // companion object
	
	//object facebooks extends MappedManyToMany(UserFBAccount, UserFBAccount.user, UserFBAccount.fb,  FBAccount)
	object accounts extends MappedManyToMany(UserAccount, UserAccount.user, UserAccount.account, Account)
	//object twitters extends MappedManyToMany(UserTwitterAccount, UserTwitterAccount.user, UserTwitterAccount.twitter,  TwitterAccount)
	object feeds extends MappedManyToMany(UserRSSSource, UserRSSSource.user, UserRSSSource.rss, RSSSource)
	
	/*
	 * Is this necessary?  I already have a LongMappedMapper in "account" and we
	 * probably won't be using the "ownership" column here.
	 * That said, it may nonetheless be helpful
	 */
	object ownership extends MappedOneToMany(Account, Account.primaryUser)
	
	object self extends LongMappedMapper(this, DiscourseObject){
		 override def dbIndexed_? = true
	}
	
	def getDiscourse = {
		DiscourseObject.find(By(DiscourseObject.id, self.is)).openTheBox
	}
	
	//TODO: actually store the map, not just the article
	//we could have another join table between relation and agents
	def appendHistory(article : Article, agents : Set[Agent]) = {
		getDiscourse.appendHistory(article)
	}
	
	def getHistory = {
		getDiscourse.getHistory(DiscourseObject.maxHistory)
	}
	
	def deleteTotally = {
		//TODO: fix synchronization issue with property and owners
		DiscourseObject.transact(Unit =>
				accounts.forall(_.delete_!) &&
				//property.forall(prop => if(prop.owners.size == 1) prop.delete_! else true) &&
				getDiscourse.delete_! && delete_!
		)
	}
}

object User extends User with MetaMegaProtoUser[User] {
	override val basePath = "user" :: Nil
	override def screenWrap = Full(<lift:surround with="boilerplate" at="content"><lift:bind /></lift:surround>)
	
	/**
	 * We add default agents and traits for users.  The agents to which a user listens may not be the same
	 * as the agents owned by that user.
	 */
	def make : User = {
		val d = DiscourseObject.make(Map[String, Any]()).discourseType(DiscourseTypes.user)
		d.addTrait("history", Set[DiscourseObject]())
		d.addTrait("agents", Set[DiscourseObject]())
		//val hearEverything = DiscourseObject.makeAgent(Map[String, Any](), "fsdajkl")
		//hearEverything.self.save
		//UniversalAgents.create.agentId(hearEverything.getId).save
		//hearEverything.self.addTrait("text", " test ")
		//d.get[RelationTraitImpl]("agents").subjects += hearEverything.self
		val u = User.create.self(d)
		//u.property += hearEverything.self
		//hearEverything.self.owners += u
		u.save
		d.save
		u
	}
}

/*class Ownership extends Mapper[Ownership]{
	def getSingleton = Ownership
	
	object user extends LongMappedMapper(this, User)
	object discourse extends LongMappedMapper(this, DiscourseObject)
}*/

//object Ownership extends Ownership with MetaMapper[Ownership]