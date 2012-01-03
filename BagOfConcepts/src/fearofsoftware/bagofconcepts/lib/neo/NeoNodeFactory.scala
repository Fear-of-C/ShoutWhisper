package fearofsoftware.bagofconcepts.lib.neo
import fearofsoftware.bagofconcepts.lib.NodeFactory
import fearofsoftware.bagofconcepts.lib.Node
import org.neo4j.kernel.EmbeddedGraphDatabase
import scala.collection.JavaConversions._
import org.neo4j.graphdb.Relationship

/**
 * Reads from neo4j
 * 
 * Probably has to cache all values in order to be safe from synchronization issues with the database.
 * For now, can we assume that the database is basically read-only?  Maybe later we can figure out
 * a way to make these classes not cause everything to implode if values suddenly change.
 * 
 * What if we wrap entire grid operations in a transaction?  Is that ok?  The problem: this still gets
 * into the same problem as "lock the database."  We might just have to live with the idea that
 * this graph is not generally writeable, or figure out a way to make algorithms sane under instability.
 * 
 * Also, transactions are in-memory, so we can't make big ones.  That said, we're also caching grid
 * clusters in memory.
 */
class NeoNode(factory : NeoNodeFactory, val node : org.neo4j.graphdb.Node) extends Node{
	
	private def getRelationshipSet = asIterable(node.getRelationships()).toSet[Relationship]
	
	val myCode = toString.hashCode
	
	def getNeighbors(threshold : Double) : Set[Node] = {
		getRelationshipSet.
			map(rel => factory.findForNode(rel.getEndNode()))
	}

	def getProbability(other : Node) : Double = {
		other match{
			case n : NeoNode => {
				val relationships = getRelationshipSet.filter(_.getEndNode == n.node)
				if(relationships.isEmpty) 0.0 else 1.0
			}
			case _ => 0.0
		}
	}

	def getLinkTotal : Double = {
		getRelationshipSet.size.toDouble
	}
	
	/**
	 * This should be proportional to link total but normalized to be a probability.
	 */
	def selfProbability : Double = {
		getLinkTotal/factory.linkTotal
	}

	def getWord : String = {
		node.getProperty("text").toString
	}
	
	override def equals(that : Any) = that match{
		case other : NeoNode => other.node == node
		case _ => false
	}
	
	override def hashCode = {
		myCode
	}
}

class NeoNodeFactory(db : EmbeddedGraphDatabase, val linkTotal : Long, val nodeTotal : Long) extends NodeFactory{
	
	def findForNode(n : org.neo4j.graphdb.Node) = {
		new NeoNode(this, n)
	}
	
	override def findForWord(word : String) : NeoNode = {
		new NeoNode(this, db.index.forNodes("terms").get("text", word).getSingle)
	}
}