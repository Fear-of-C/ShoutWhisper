package fearofsoftware.bagofconcepts.lib.test

import fearofsoftware.bagofconcepts.lib._

class TestNode(factory : TestNodeFactory, word : String) extends Node{
	
	private val cacheString = "N:" + getWord
	private val cacheCode = cacheString.hashCode
	
	override def getNeighbors(threshold : Double) : Set[Node] = {
		factory.links(this).filter(_._2 > threshold).keys.toSet - this
	}

	override def getProbability(other : Node) : Double = {
		factory.links(this)(other)
	}

	override def getLinkTotal : Double = {
		factory.links(this).values.sum
	}

	override def selfProbability : Double = {
		getLinkTotal/factory.entireGraphCount
	}

	override def getWord : String = {
		word
	}
	
	override def toString = {
		cacheString
	}
	
	override def hashCode = {
		cacheCode
	}
}