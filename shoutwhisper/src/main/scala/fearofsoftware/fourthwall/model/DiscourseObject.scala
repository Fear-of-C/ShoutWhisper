package fearofsoftware.fourthwall.model

import net.liftweb.mapper._
import java.util.Date
import net.liftweb.common.Box
import net.liftweb.common.Full
import net.liftweb.db.DB
import net.liftweb.common.EmptyBox
import net.liftweb.common.Empty

trait Discourse{
	def apply(name : String) : Any
	def has(tr : String) : Boolean
	def get[T](name : String) : T
	def displayableValue(name : String) : String
	def getId : Long
	def findTrait(name : String) : Box[Trait]
	def self : DiscourseObject
}

object DiscourseTypes{
	val none = 0
	val agent = 1
	val article = 2
	val user = 3
}

class DiscourseObject extends LongKeyedMapper[DiscourseObject] with ManyToMany with IdPK with Agent with Article with TransactSaveable with OneToMany[Long, DiscourseObject]{

	def getSingleton = DiscourseObject
	
	def maxHistory = 500
	
	//object owners extends MappedManyToMany(Ownership, Ownership.discourse, Ownership.user, User)
	object traits extends MappedOneToMany(Trait, Trait.discourse)
	object discourseType extends MappedInt(this)
	object uses extends MappedManyToMany(Relation, Relation.subject, Relation.tr, RelationTraitImpl)
	
	def findTrait(name : String) : Box[Trait]= {
		val traitList = Trait.findAllByPreparedStatement((conn : SuperConnection) => {
			val stmt = conn.connection.prepareStatement(
			"SELECT Trait.* FROM TraitName JOIN Trait ON " +
			"TraitName.id=Trait.name WHERE " + 
			"TraitName.name=? AND " +
			"Trait.discourse=?;")
			stmt.setString(1, name)
			stmt.setLong(2, getId)
			stmt
		})
		if(traitList.isEmpty) Empty else Box(traitList.first)
	}
	
	def allTraits : Set[String]= {
		TraitName.findAllByPreparedStatement((conn : SuperConnection) => {
			val stmt = conn.connection.prepareStatement(
			"SELECT TraitName.* FROM TraitName JOIN Trait ON " +
			"TraitName.id=Trait.name WHERE " +
			"Trait.discourse=?;")
			stmt.setLong(1, getId)
			stmt
		}).map(_.name.is).toSet
	}
	
	override def toString : String = { allTraits.toString }
	
	
	def has(tr : String) = {
		!findTrait(tr).isEmpty
	}
	
	def get[T](name : String) : T = {
		apply(name).asInstanceOf[T]
	}
	
	def getId = this.id.is.toLong
	
	def apply(name : String) : Any = {
		findTrait(name) match{
			case Full(tr) => tr.getValue
			/*
			 * This happens when you delete a universal agent
			 */
			case _ => throw new NoSuchElementException("Object " + this + " doesn't have trait " + name)
		}
	}
	
	def displayableValue(name : String) : String = {
		apply(name).toString
	}
	
	def addTrait(name : String, value : Any) = {
		if(value == null){
			false
		}
		else{
			traits += Trait.makeTrait(this, name, value)
			save
		}
	}
	
	def nestedAddTrait(name : String, value : Any) = {
		if(value == null){
			false
		}else{
			traits += Trait.nestedMakeTrait(this, name, value)
		}
	}
	
	/**
	 * Removes a trait from this object.  May prune the trait name.
	 * 
	 * We should start an end a transaction here.  That way, we will
	 * never have partially delete objects in the table.
	 */
	def remove(name : String) = {
		findTrait(name).openTheBox.delete_!
	}
	
	/**
	 * Must ensure that we delete all trait entries, plus any trait names that
	 * henceforth go unused.  Could cause headaches if it fails to serialize
	 * with trait creation.  We can use a synchronized block for this and later
	 * look into making it transactional through mysql.
	 * 
	 * What we really want is some sort of cascading delete.
	 * 
	 * We delete
	 * -the trait implementation entries corresponding to this thing's traits.
	 * -the trait entry corresponding to this thing's traits
	 * -any orphaned trait names
	 * 
	 * We need recursion to isolate the implemenation of those other classes.
	 */
	override def delete_! = {
		DiscourseObject.transact(
				Unit =>{
					nestedDelete
				}
		)
	}
	
	override def nestedDelete = {
		Trait.findAll(By(Trait.discourse, id.is)).forall(_.nestedDelete) && selfDelete
	}
	
	/**
	 * Must save transactionally
	 */
	override def save = {
		DiscourseObject.transact(
				Unit => {
					nestedSave
				}
		)
	}
	
	override def selfDelete = super.delete_!
	
	/**
	 * Does not necessarily start a new transaction.  Does not ensure
	 */
	override def selfSave = super.save
	
	/**
	 * Doesn't start a new transaction but does ensure that sub-objects wind up saved properly.
	 */
	override def nestedSave = traits.forall(_.save) && selfSave
}

object DiscourseObject extends DiscourseObject with LongKeyedMetaMapper[DiscourseObject]{
		
	def makeAgent(traits : Map[String, Any], search : String, owners : Set[DiscourseObject]) : Agent = {
		val a = make(traits
				+ ("history" -> Set[DiscourseObject]())
				+ ("text" -> search)
				+ ("owners" -> owners)
		).discourseType(DiscourseTypes.agent)
		//made the discourse object; now to add the agentness to it
		//TODO: make this part of the same transaction if possible and sensible to do so
		transact(Unit => {
			a.becomeSearchable
			owners.forall(owner => {
				val relation = owner.get[RelationTraitImpl]("agents")
				relation.subjects += a
				relation.selfSave
			})
			a.selfSave})
		println("Agent created: " + a + " with owner " + a("owners"))
		a
	}
	
	def transact(f : Unit => Boolean) : Boolean = {
		DB.use(DefaultConnectionIdentifier) { conn =>
			conn.setAutoCommit(false)
			var success = false
			success = f()
			if(!(conn.getAutoCommit))
				if(success) conn.commit else {
					conn.rollback
					throw new IllegalStateException("Transaction failed.")
				}
			conn.setAutoCommit(true)
			success
		}
	}
	
	def make(traits : Map[String, Any]) : DiscourseObject = {
		val d = DiscourseObject.create.discourseType(DiscourseTypes.none)
		d.save
		//by default, give everything a created date
		d.nestedAddTrait("date created", new Date)
		//then look to the map for defining other traits
		traits.foreach(tr => d.nestedAddTrait(tr._1, tr._2))
		if(d.selfSave) d else throw new IllegalStateException("Couldn't create discourse object.")
	}
	
	def makeArticle(traits : Map[String, Any]) = {
		val article = make(traits + ("matched to" -> Set[DiscourseObject]())).discourseType(DiscourseTypes.article)
		if(article.selfSave) article else throw new IllegalStateException("Couldn't save article.")
	}
	
	def checkGC(id : Long) = {
		synchronized{
			DiscourseObject.find(By(DiscourseObject.id, id)) match{
				case Full(o) => if(o.uses.isEmpty) o.delete_! else true
				case _ => true
			}
			
		}
	}
}















