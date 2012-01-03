package fearofsoftware.bagofconcepts.lib

import scala.collection.immutable.Set

trait Node {
	
	def getNeighbors(threshold : Double) : Set[Node]

	def getProbability(other : Node) : Double

	def getLinkTotal : Double
	
	/**
	 * This should be proportional to link total but normalized to be a probability.
	 */
	def selfProbability : Double

	def getWord : String
	
	override def equals(that : Any) = that match{
		case other : Node => other.toString == toString
		case _ => false
	}
	
	override def hashCode = {
		toString.hashCode
	}
	
	override def toString : String = {
		"N:" + getWord
	}
}
	
abstract class NodeFactory{
  def findForWord(word : String) : Node
}