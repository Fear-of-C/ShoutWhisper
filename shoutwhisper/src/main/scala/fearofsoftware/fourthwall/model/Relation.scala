package fearofsoftware.fourthwall.model

import net.liftweb.mapper._

class RelationTraitImpl extends LongKeyedMapper[RelationTraitImpl] with OneToMany[Long, RelationTraitImpl] with ManyToMany with TransactSaveable with IdPK{

	def getSingleton = RelationTraitImpl
	
	object tr extends LongMappedMapper(this, Trait){
		 override def dbIndexed_? = true
	}
	
	object relations extends MappedOneToMany(Relation, Relation.tr)
	
	object subjects extends MappedManyToMany(Relation, Relation.tr, Relation.subject, DiscourseObject)
	
	/**
	 * Returns things in the order in which they were added/saved.
	 */
	def getAsList = {
		Relation.findAll(By(Relation.tr, id.is), OrderBy(Relation.id, Ascending))
	}
	
	def getValuesAsList = {
		DiscourseObject.findAllByPreparedStatement(conn => {
			val ps = conn.prepareStatement(
					"SELECT DiscourseObject.* FROM " +
					"DiscourseObject JOIN Relation ON " +
					"DiscourseObject.id = Relation.subject " +
					"WHERE Relation.tr = ?")
			ps.setLong(1, id.is)
			ps
		})
	}
	
	def getHead(count : Int) : List[Relation]= {
		getAsList.take(count).toList
	}
	
	/**
	 * Make sure that these get added in specified order.
	 */
	def appendList(objs : List[DiscourseObject]) = {
		val array = objs.toArray
		for(i <- 0 until array.size){
			array(i).selfSave
			subjects += array(i)
		}
		save
	}
	
	def append(obj : DiscourseObject) = {
		appendList(obj :: Nil)
	}
	
	def addAll(objs : Collection[DiscourseObject]) = {
		//println("Adding collection " + objs)
		objs.foreach(_.selfSave)
		save
		//relations ++= objs.filterNot(subjects.contains(_)).map(sub => Relation.create.tr(this).subject(sub))
		subjects ++= objs//.filterNot(subjects.contains(_))
		save
		//println("Should have added " + subjects.toSet)
		save // -- This line of code is what's erasing the objects
		objs.foreach(_.selfSave)
		save
		subjects.foreach(_.selfSave)
		save
		//println("Should have added " + subjects.toSet)
		true
	}
	
	def add(obj : DiscourseObject) = {
		addAll(Set[DiscourseObject](obj))
	}
	
	override def selfDelete = super.delete_!
	
	override def nestedDelete = {
		val subjectSet = subjects.toSet
		subjects.clear
		subjectSet.foreach(subject => DiscourseObject.checkGC(subject.id.is))
		//relations.clear
		//TODO: find out whether we need to explicitly clear these lists
		selfDelete
	}
	
	override def delete_! = {
		DiscourseObject.transact(
				Unit =>{
					nestedDelete
				}
		)
	}
	
	override def save = {
		DiscourseObject.transact(
				Unit =>{
					nestedSave
				}
		)
	}
	
	override def selfSave = super.save
	
	override def nestedSave = relations.forall(_.save) && super.save
}

object RelationTraitImpl extends RelationTraitImpl with LongKeyedMetaMapper[RelationTraitImpl]{
	
}

/**
 * Represents a directed graph link.  No need for predicates here, as traits already are predicates.
 */
class Relation extends LongKeyedMapper[Relation] with IdPK{
	def getSingleton = Relation
	
	object tr extends LongMappedMapper(this, RelationTraitImpl){
		 override def dbIndexed_? = true
	}
	object subject extends LongMappedMapper(this, DiscourseObject){
		 override def dbIndexed_? = true
	}
}

object Relation extends Relation with LongKeyedMetaMapper[Relation]{
	
}