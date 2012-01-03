package fearofsoftware.fourthwall.model
import net.liftweb.mapper.By
import net.liftweb.mapper.BySql
import java.util.Date
import net.liftweb.db.DB
import net.liftweb.mapper.ByList
import net.liftweb.mapper.OrderBy
import net.liftweb.mapper.Descending
import net.liftweb.mapper.Ascending

/**
 * Mixin.  Gives the agent-based convenience methods to discourse objects.
 */
trait Agent extends Discourse{
	override def self = this.asInstanceOf[DiscourseObject]
		
	def isAgent = {
		has("text")
	}
	
	private def findHistory : RelationTraitImpl = {
		val tr = findTrait("history").openTheBox.id.is
		RelationTraitImpl.find(By(RelationTraitImpl.tr, tr)).openTheBox
	}
	
	/**
	 * Adding history into the table.  Gotta watch out for inconsistencies between inserts,
	 * but we should be find as long as we make sure that things don't get deleted in the
	 * interim.
	 * 
	 * We need to get the 2 ends of a relation.  One of those comes from finding a trait object.
	 */
	def appendHistory(article : Article) : Unit = {
		DiscourseObject.transact(Unit => {
			if(!has("history")){
				return true
			}
			val relationImpl = findHistory
			val relation = Relation.create.subject(article.self)
			relationImpl.relations += relation
			//this is the dangerous line - with any luck, Lift maps this down to something thread safe
			//because in its implementation, we are using a foreign key and NOT a set
			//if it gets really bad, then we synchronize the last couple of lines - that will prevent
			//multiple things from adding to the same agent's history
			
			relationImpl.save && relation.save && pruneHistory
		})
	}
	
	def cutoff = {
		self.get[Long]("history position") - DiscourseObject.maxHistory
	}
	
	/**
	 * 1st, note that the RelationTraitImpl already contains some logic for how to handle
	 * ordering - this is why relations have primary keys.
	 */
	private def pruneHistory = {
		val relTr = findHistory
		val allResults = Relation.findAll(By(Relation.tr, relTr.id.is), OrderBy(Relation.id, Ascending))
		if(allResults.size - DiscourseObject.maxHistory > 0){
		  try{
			allResults.take(allResults.size.toInt - DiscourseObject.maxHistory.toInt).toList.forall(rel => {
				val objId = rel.subject
				//order of the below matters: otherwise will fail to GC properly
				rel.delete_! && DiscourseObject.checkGC(objId)
			})
		  }
		  catch{
		    case npe: NullPointerException =>
		      allResults.take(allResults.size.toInt - DiscourseObject.maxHistory.toInt).toList.foreach(rel => 
					println("Relation: " + rel.subject + " Found: " + DiscourseObject.find(By(DiscourseObject.id, rel.subject))))
				false
		    case _ => false
		  }
		} else true
	}
	

	def getHistory(max : Int) : List[Article] = {
		findHistory.getValuesAsList.take(max)
	}
	
	/**
	 * Called once on a new agent to make it searchable.  We should upgrade this method later
	 * such that it can be called more than once if the agent gets updated.
	 * 
	 */
	def becomeSearchable : Unit = {
		DiscourseObject.transact(Unit => {
			if(!has("text")){
				return true
			}
			val text = self.get[String]("text")
			val words = text.split("\\s+")
			words.size match{
				case 0 => {
					//then we don't do anything, because there's nothing for which to search.
					true
				}
				case _ => {
					DiscourseObject.transact(Unit => {
						val wordUses = Range(0, words.size).map(i => (words(i), i)).toMap
						val wordObjs = words.toSet.map((word : String) => (word, Word.ensureWord(word))).toMap
						val textTrait = TextTraitImpl.find(By(TextTraitImpl.tr, self.findTrait("text").openTheBox.id.is)).openTheBox
						wordUses.foreach{case (word, pos) => {
							WordUse.create.position(pos).text(textTrait.id.is).word(wordObjs(word).id.is).save
						}}
						self.save
					})
				}
			}
		})
	}
}