package fearofsoftware.bagofconcepts.lib.test

import fearofsoftware.bagofconcepts.lib._

import scala.collection.JavaConversions.JConcurrentMapWrapper
import java.util.concurrent.ConcurrentHashMap

class TestNodeFactory(init : Array[Array[Double]]) extends NodeFactory{
	
	
	val links : scala.collection.mutable.Map[Node, scala.collection.mutable.Map[Node, Double]] =
		new JConcurrentMapWrapper(new ConcurrentHashMap[Node, scala.collection.mutable.Map[Node, Double]])

	val nodes : scala.collection.mutable.Map[String, Node] = 
		new JConcurrentMapWrapper(new ConcurrentHashMap[String, Node])
	
	initializeFromMatrix(init)
	
	def entireGraphCount = {
		links.map(_._2.values.sum).sum/2.0
	}
	
	override def findForWord(word : String) : Node = {
		nodes(word)
	}
	
	private def initializeFromMatrix(matrix : Array[Array[Double]]) = {
		(matrix.size :: matrix.indices.toList).foreach((n : Int) => {
			val name = "n" + n
			nodes.put(name, new TestNode(this, name))
			links.put(nodes(name), new JConcurrentMapWrapper(new ConcurrentHashMap[Node, Double]))
			links(nodes(name)).put(nodes(name), 1.0)
		})
		matrix.indices.foreach((i : Int) => {
			val iname = "n" + i
			matrix(i).indices.foreach((j : Int) =>{
				val jname = "n" + (j + i + 1)
				links(nodes(iname)).put(nodes(jname), matrix(i)(j))
				links(nodes(jname)).put(nodes(iname), matrix(i)(j))
			})
		})
	}
	
	
}