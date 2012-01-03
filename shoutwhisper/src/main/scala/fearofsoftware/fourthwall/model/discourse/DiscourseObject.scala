package fearofsoftware.fourthwall.model.discourse
import org.neo4j.graphdb.Node
import org.neo4j.graphdb.Transaction
import scala.collection.JavaConversions._
import net.liftweb.common.Box
import net.liftweb.common.Full
import net.liftweb.common.Empty
import org.neo4j.helpers.collection.MapUtil
import org.neo4j.graphdb.Direction
import org.neo4j.index.lucene.QueryContext
/**
 * Discourse Objects implemented through Neo4j:
 * 
 * options:
 * 1) Create a wrapper class around Neo4j nodes.  Accept some overhead
 * and overhead code for translating between discourse system and Neo4j.
 * 2) Use Neo4j's nodes directly.
 * 
 * Going to favor #1 for now, for purposes of modularity.
 * 
 * Do discourse objects naturally have other discourse objects as traits?  This seems like the right
 * idea, but we could also say that traits are simply values and not allow them to.
 * 
 * In order to support relationship finding of this sort, we will probably need a relationship index.
 */
class DiscourseObject(val node : Node){
	
	def get[T](name : String) : Box[T]= {
		apply(name) match{
			case Full(obj) => if(obj.isInstanceOf[T]) Box[T](obj.asInstanceOf[T]) else Empty
			case _ => Empty
		}
	}
	
	def transact(func : Unit => Unit) = {
		val tx = node.getGraphDatabase.beginTx();
		try
		{
			func()
		    tx.success
		}
		finally
		{
		    tx.finish
		}
	}
	
	def getRelationships(name : String) = {
		val query = get[Long](countProperty(name)) match{
			case Full(count) => new QueryContext(relIdent(name)).sort( "num" )
			case _ => new QueryContext(relIdent(name))
		}
		node.getGraphDatabase.index.forRelationships("links", MapUtil.stringMap("type", "exact")).query(DiscourseObject.REL_IDENT, query).
			iterator.map(new DiscourseObject.Relationship(_))
	}

	def apply(name : String) : Box[Any] = {
		if(node.hasProperty(name)){
			node.getProperty(name) match{
				case DiscourseObject.RELATIONSHIP => {
					Box[Any](getRelationships(name))
				}
				case v : Any => Box[Any](v)
			}
			
		} else Empty
	}
	
	private def relIdent(name : String) = {
		name + node.getId
	}
	
	private def propertyIndex = {
		node.getGraphDatabase.index.forNodes(DiscourseObject.PROPERTY_INDEX, MapUtil.stringMap("type", "fulltext"))
	}
	
	/**
	 * This method returns all traits, including those that could be links to other discourse objects.
	 */
	def allProperties : Set[String]= {
		node.getPropertyKeys().toSet
	}
	
	private def relStartProperty(name : String ) = "_rel_start_" + name
	private def countProperty(name : String) = "_rel_count_" + name
	
	private def appendNode(name : String, d : DiscourseObject, seq : Boolean) = {
		val rel = node.createRelationshipTo(d.node, DiscourseRelationshipTypes.LINK)
		val identName = relIdent(name)
		rel.setProperty(DiscourseObject.REL_IDENT, identName)
		rel.setProperty(DiscourseObject.REL_FROM, node.getId)
		rel.setProperty(DiscourseObject.REL_TO, d.node.getId)
		val relIndex = node.getGraphDatabase.index.forRelationships(DiscourseObject.RELATIONSHIP_INDEX)
		relIndex.add(rel, DiscourseObject.REL_FROM, node.getId)
		relIndex.add(rel, DiscourseObject.REL_TO, d.node.getId)
		if(seq){
			val count : Long = (get[Long](countProperty(name)) openOr 0L) + 1L
			rel.setProperty("num", count)
			relIndex.add(rel, "num", count)
		}
		relIndex.add(
				rel, DiscourseObject.REL_IDENT, identName)
	}
	
	private def downToPosition(name : String, pos : Int) = {
		getRelationships(name).take(pos).toList
	}
	
	private def removeNode(name : String, d : DiscourseObject) = {
		val hits = node.getGraphDatabase.index.forRelationships(DiscourseObject.RELATIONSHIP_INDEX).
			query("from:" + node.getId + " AND to:" + d.node.getId + " AND " + DiscourseObject.REL_IDENT + ":" + relIdent(name))
		hits.iterator.foreach(hit => hit.delete)
	}
	
	private def deleteLast(name : String) = {
		val rels = getRelationships(name)
		var rel : DiscourseObject.Relationship = null
		while(rels.hasNext){
			rel = rels.next
		}
		rel.delete
	}
	
	/**
	 * Appends a new item to a relationship.
	 */
	def append(name : String, value : DiscourseObject) = transact(Unit => {
		appendNode(name, value, apply(name) match{
			case Full(DiscourseObject.RELATIONSHIP) => false
			case Full(DiscourseObject.SEQUENCE) => true
			case _ => throw new IllegalArgumentException(this + " has no appendable property with " + name)
		})
	})
	
	/**
	 * Sets the value of a property. Guaranteed to move from old to new value in a transactional manner.
	 */
	def set(name : String, value : Any) = transact(Unit => {
		apply(name) match{
			case Full(item) => remove(name)
			case _ => {}
		}
		def relationshipCreation(v : Traversable[DiscourseObject]) = {
			node.setProperty(name, DiscourseObject.RELATIONSHIP)
			node.setProperty(countProperty(name), 0)
			v.foreach(target => {
				appendNode(name, target, v.isInstanceOf[Seq[DiscourseObject]])
			})
		}
		value match{
			case v : DiscourseObject => relationshipCreation(Set[DiscourseObject](v))
			case v : Traversable[DiscourseObject] => relationshipCreation(v)
			case _ =>  node.setProperty(name, value)
		}
		get[Boolean](DiscourseObject.SEARCHABLE) match{
			case Full(true) => {
				propertyIndex.add(node, name, node.getProperty(name))
			}
			case _ => {}
		}
	})
	
	def remove(name : String) = transact(Unit => {
		node.removeProperty(name) match {
				case DiscourseObject.RELATIONSHIP => {
					getRelationships(name).foreach(_.delete)
				}
				case _ => {}
			}
			get[Boolean](DiscourseObject.SEARCHABLE) match{
				case Full(true) => {
					propertyIndex.remove(node, name, node.getProperty(name))
				}
				case _ => {}
			}
		})
	
	/**
	 * Adds itself to the full text index.  In fact, we are probably adding all traits to full
	 * text indices.  How, precisely, does this even work?  Do we have a global set of traits?
	 * 
	 * We could have DiscourseObject keep track of a full set of trait names with indices.  That would
	 * allow us to build an index for each trait.
	 * 
	 */
	def becomeSearchable = transact(Unit => {
		get[Boolean](DiscourseObject.SEARCHABLE) match{
			case Full(true) => {}
			case _ => {
				node.setProperty(DiscourseObject.SEARCHABLE, true)
				allProperties.foreach(name => {
					val index = node.getGraphDatabase.index.forNodes(DiscourseObject.PROPERTY_INDEX)
					index.add(node, name, node.getProperty(name))
				})
			}
		}
	})
	
	/**
	 * We want to make a search query over all traits?  Hard to tell.  Maybe we figure that
	 * one out later.  How do we decide which traits are relevant?  Maybe we can assume that
	 * any searchable trait is relevant - agents will drop irrelevant traits.
	 * 
	 * In theory, a fulltext index can support a lot of these searches rather trivially.
	 * 
	 * http://docs.neo4j.org/chunked/1.4/indexing-lucene-extras.html describes how to query
	 * over numeric ranges.
	 * 
	 * We still need to figure out how to set up inverse queries with ranges, as the range will
	 * probably appear on the agent side.  Maybe a custom data structure with fulltext search
	 * could express a range as something resembling a suffix tree.
	 */
	def wordBagSearch : Map[String, Int]= {
		get[String]("text") match{
			case Full(text) => {
				val wordBag = scala.collection.mutable.Map[String, Int]()
				def wordFound(word : String) = wordBag synchronized(wordBag.put(word, if(wordBag.contains(word)) wordBag(word) + 1 else 0))
				text.split("\\s+").map(_.replace("[^a-zA-Z0-9]", ""))foreach(wordFound(_))
				wordBag.toMap
			}
			case _ => Map[String, Int]()
		}
	}
	def delete = transact(Unit => node.delete)
}

object DiscourseObject {
	val RELATIONSHIP = new java.lang.Integer(0)
	val SEQUENCE = new java.lang.Integer(1)
	
	val RELATIONSHIP_INDEX = "relationships"
	val PROPERTY_INDEX = "properties"
	val REL_IDENT = "relIdent"
	val SEARCHABLE = "searchable"
	val REL_FROM = "from"
	val REL_TO = "to"
	
	/**
	 * Wraps a Neo4j relationship, allowing manipulation on relationships.
	 */
	class Relationship(r : org.neo4j.graphdb.Relationship){
		def delete = {
			r.delete
		}
	}
	
	def checkGC(d : DiscourseObject) = synchronized {
		d.transact(Unit => 
			if(d.node.getRelationships(Direction.INCOMING).isEmpty){
				d.delete
				true
		} else false)
	}
}