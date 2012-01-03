package fearofsoftware.fourthwall.model

import net.liftweb.mapper._


class TextTraitImpl extends LongKeyedMapper[TextTraitImpl] with IdPK with OneToMany[Long, TextTraitImpl] with TransactSaveable{

	def getSingleton = TextTraitImpl
	
	object tr extends LongMappedMapper(this, Trait){
		 override def dbIndexed_? = true
	}
	object words extends MappedOneToMany(WordUse, WordUse.text, OrderBy(WordUse.position, Ascending))
	
	//inverted index is seperate
	object value extends MappedText(this)
	
	override def delete_! = {
		DiscourseObject.transact(
				Unit =>{
					nestedDelete
				}
		)
	}
	
	override def selfDelete = super.delete_!
	
	override def nestedDelete = {
		words.forall(_.nestedDelete) && selfDelete
	}
	
	override def save = {
		DiscourseObject.transact(
				Unit =>{
					nestedSave
				}
		)
	}
	
	override def selfSave = super.save
	
	override def nestedSave = words.forall(_.save) && selfSave
}

object TextTraitImpl extends TextTraitImpl with LongKeyedMetaMapper[TextTraitImpl]{
	
}