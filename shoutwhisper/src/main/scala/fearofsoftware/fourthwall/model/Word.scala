package fearofsoftware.fourthwall.model

import net.liftweb.mapper._
import net.liftweb.common.Full

class Word extends LongKeyedMapper[Word] with OneToMany[Long, Word] with IdPK with TransactSaveable{

	def getSingleton = Word
	
	object text extends MappedText(this){
		 override def dbIndexed_? = true
	}
	
	object uses extends MappedOneToMany(WordUse, WordUse.word, OrderBy(WordUse.position, Ascending))
}

object Word extends Word with LongKeyedMetaMapper[Word]{
	
	def pruneWord(id : Long) = {
		synchronized {
			find(By(Word.id, id)).filter(_.uses.isEmpty).forall(_.delete_!)
		}
	}
	
	def ensureWord(string : String) = {
		synchronized {
			find(By(Word.text, string)) match{
				case Full(word) => word
				case _ => {
					val word = Word.create.text(string)
					if(word.save) word else null
				}
			}
				
		}
	}
}

/**
 * This is a meta-trait.  It has many values for a given discourse object and word.  It can be
 * broken back down into a text object if needed.  We might even call this a trait for bag-of-traits.
 */
class WordUse extends LongKeyedMapper[WordUse] with TransactSaveable with IdPK{

	def getSingleton = WordUse
	
	object word extends LongMappedMapper(this, Word){
		 override def dbIndexed_? = true
	}
	object position extends MappedLong(this)
	
	object text extends LongMappedMapper(this, TextTraitImpl){
		 override def dbIndexed_? = true
	}
	
	override def selfDelete = super.delete_!
	
	override def nestedDelete = {
		val textImpl = TextTraitImpl.find(By(TextTraitImpl.id, text)).openTheBox
		textImpl.words -= this
		textImpl.selfSave && selfDelete && Word.pruneWord(word)
	}
	
	override def delete_! = {
		DiscourseObject.transact(
				Unit =>{
					 nestedDelete
				}
		)
	}
}

object WordUse extends WordUse with LongKeyedMetaMapper[WordUse]{
	
}