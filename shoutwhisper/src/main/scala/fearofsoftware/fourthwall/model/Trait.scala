package fearofsoftware.fourthwall.model

import net.liftweb.mapper._
import net.liftweb.common.Full
import net.liftweb.common.Box
import java.util.Date
import scala.collection.JavaConversions.JConcurrentMapWrapper
import java.util.concurrent.ConcurrentHashMap

object Implementations{
	val text = 0
	val date = 1
	val relation = 2
	val link = 3
	val integer = 4
	
}

class TraitName extends LongKeyedMapper[TraitName] with ManyToMany with OneToMany[Long, TraitName] with IdPK with TransactSaveable{

	def getSingleton = TraitName
	
	object name extends MappedText(this){
		 override def dbIndexed_? = true
	}
	
	object vals extends MappedOneToMany(Trait, Trait.name)
	
}

object TraitName extends TraitName with LongKeyedMetaMapper[TraitName]{
	
	private def CACHE_SIZE = 100
	private val cache = new JConcurrentMapWrapper[String, TraitName](new ConcurrentHashMap[String, TraitName](CACHE_SIZE))
	
	def ensureTraitName(name : String) : TraitName = {
		if(!cache.contains(name)){
			synchronized{
				val t = TraitName.find(By(TraitName.name, name)) match {
					case Full(tn) => tn
					case _ => {
						val tn = TraitName.create.name(name)
						if(tn.save) tn else null
					}
				}
				//this way, we won't have double-caching ever
				if(!cache.contains(name) && cache.size < CACHE_SIZE){
					cache.put(name, t)
				}
				t
			}
		} else return cache(name)
	}
	
	def pruneTraitName(id : Long) : Boolean = {
		synchronized{
			TraitName.find(By(TraitName.id, id)).filter(_.vals.isEmpty).forall(tn => {
				if(cache.contains(tn.name)){
					cache.remove(tn.name)
				}
				tn.delete_!
			})
		}
	}
}

/*
 * What is a trait?  It's something by which a discourse object can map to a value, like an RDF
 * predicate.  This means it's a 3-way pairing of discourse object, a name, and a value.  A
 * discourse object has many trait names, but one value for each.  The only forbidden condition
 * is having 2 traits with the same name and object but different values.
 * 
 * We need somehow that the join table is keyed by name and object but allows fast indexing
 * with something else.  If we use ids from the implemenations, then we separate this table from them,
 * which is really quite nice.  We would still like to know, however, what implementations are available.
 * 
 * We can have a flag for use in code, which specifies how to find the implementation for a trait.
 * 
 * There is a kind of trait value that can be a collection of discourse objects or a single discourse
 * object.
 */

class Trait extends LongKeyedMapper[Trait] with IdPK with TransactSaveable{
	def getSingleton = Trait
	
	object name extends LongMappedMapper(this, TraitName)
	object discourse extends LongMappedMapper(this, DiscourseObject)
	object implType extends MappedInt(this)
	
	def findTraitName = {
		TraitName.find(By(TraitName.id, name.is)).openTheBox
	}
	
	def getImplementation = {
		implType.is match {
			case Implementations.text => TextTraitImpl
			case Implementations.date => DateTraitImpl
			case Implementations.relation => RelationTraitImpl
			case Implementations.link => LinkTraitImpl
			case Implementations.integer => NumberTraitImpl
		}
	}
	
	def getImpl : Box[LongKeyedMapper[_]] = {
		implType.is match {
			case Implementations.text => TextTraitImpl.find(By(TextTraitImpl.tr, id.is))
			case Implementations.date => DateTraitImpl.find(By(DateTraitImpl.tr, id.is))
			case Implementations.relation => 
				RelationTraitImpl.find(By(RelationTraitImpl.tr, id.is))
			case Implementations.link => LinkTraitImpl.find(By(LinkTraitImpl.tr, id.is))
			case Implementations.integer => NumberTraitImpl.find(By(NumberTraitImpl.tr, id.is))
		}
	}
	
	def getValue : Any = {
		implType.is match {
			case Implementations.text => TextTraitImpl.find(By(TextTraitImpl.tr, id.is)).openTheBox.value.is
			case Implementations.date => DateTraitImpl.find(By(DateTraitImpl.tr, id.is)).openTheBox.value.is
			case Implementations.relation => 
				RelationTraitImpl.find(By(RelationTraitImpl.tr, id.is)).openTheBox
			case Implementations.link => DiscourseObject.find(
					By(DiscourseObject.id, LinkTraitImpl.find(By(RelationTraitImpl.tr, id.is)).openTheBox.value.is)).openTheBox
			case Implementations.integer => NumberTraitImpl.find(By(NumberTraitImpl.tr, id.is)).openTheBox.value.is
		}
	}
	
	override def delete_! = {
		DiscourseObject.transact(
			Unit =>{
				nestedDelete
			})
	}
	
	override def selfDelete = super.delete_!
	
	override def nestedDelete = {
		val tempName = name
		getImpl.openTheBox.asInstanceOf[TransactSaveable].nestedDelete && selfDelete && TraitName.pruneTraitName(name)
	}
	
	override def save = {
		DiscourseObject.transact(
			Unit =>{
				 nestedSave
			})
	}
	
	override def selfSave = super.save
	
	override def nestedSave = selfSave && (getImpl match{
					case Full(impl) => impl.save
					case _ => true
				})
}

object Trait extends Trait with LongKeyedMetaMapper[Trait]{
	
	def nestedMakeTrait(d : DiscourseObject, name : String, rawValue : Any) = {
		val traitName = TraitName.ensureTraitName(name)
		var value = rawValue
		val tr = Trait.create.name(traitName).discourse(d).implType(
			value match{
				case v : String => Implementations.text
				case v : Date => Implementations.date
				case v : Collection[DiscourseObject] => Implementations.relation
				case v : DiscourseObject => Implementations.link
				case v : Long => Implementations.integer
				case v : Int => Implementations.integer
				case _ => {
					value = rawValue.toString()
					Implementations.text
				}
			})
		tr.selfSave
		def createNumber(v : Long) = {
			val num = NumberTraitImpl.create.tr(tr).value(v)
			num.nestedSave
		}
		if(!(value match {
			case v : String => {
				val txt = TextTraitImpl.create.value(v).tr(tr)
				txt.nestedSave
			}
			case v : Date => {
				val date = DateTraitImpl.create.value(v).tr(tr)
				date.nestedSave
			}
			case v : Collection[DiscourseObject] => {
				val rel = RelationTraitImpl.create.tr(tr)
				v.foreach(_.selfSave)
				//things need IDs before we can link to them
				v match{
					case l : List[DiscourseObject] => rel.appendList(l)
					case _ => rel.addAll(v)
				}
				rel.nestedSave
				//if(name == "owners")
					//println("Created relationship " + name + " and set linked as " + rel.subjects + " from " + v + " from " + rawValue)
				rel.nestedSave
			}
			case v : DiscourseObject => {
				val link = LinkTraitImpl.create.tr(tr).value(v)
				link.nestedSave
			}
			case v : Long => createNumber(v)
			case v : Int => createNumber(v)
			case _ =>{
				val obj = TextTraitImpl.create.value(value.toString).tr(tr)
				obj.nestedSave
			}
		})) throw new IllegalStateException("Failed to save impl for new trait.")
		tr.nestedSave
		tr
	}

	
	def makeTrait(d : DiscourseObject, name : String, rawValue : Any) = {
		var tr : Trait = null
		DiscourseObject.transact(Unit => {
			tr = nestedMakeTrait(d, name, rawValue)
			true
			}
		)
		tr
	}
}