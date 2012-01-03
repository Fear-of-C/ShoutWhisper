package fearofsoftware.mainsite.model

import net.liftweb.mapper._

class User extends MegaProtoUser[User] {
	def getSingleton = User // companion object
	
	
}

object User extends User with MetaMegaProtoUser[User] with OneToMany[Long, User]{
	override val basePath = "user" :: Nil
	//override def screenWrap = Full(<lift:surround with="default" at="content"><lift:bind /></lift:surround>)
}