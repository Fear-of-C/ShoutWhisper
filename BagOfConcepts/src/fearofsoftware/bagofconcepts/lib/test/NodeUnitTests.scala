package fearofsoftware.bagofconcepts.lib.test

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import fearofsoftware.bagofconcepts.lib._

import scala.util.Random

class NodeUnitTests extends AssertionsForJUnit{

	/**
	 * Things to check:
	 * 1) Grid convergence (which might not be feasible, since we don't know
	 * which representatives it will give us.
	 */
	@Test def graphSanity() {

		//branching clusters, for comparison purposes
		val clusterBranch : Array[Array[Double]] =
			Array(Array(0.9, 0.8, 0.0, 0.1, 0.2, 0.8),
				       Array(0.9, 0.0, 0.0, 0.0, 0.0),
				            Array(0.0, 0.1, 0.1, 0.0),
				                 Array(0.7, 0.0, 0.8),
				                      Array(0.0, 0.9),
				                           Array(0.0)
				                           			)
		
		//single cluster
		val clusterAlone : Array[Array[Double]] =
			Array(Array(0.9, 0.8, 0.0, 0.1, 0.2, 0.0),
				       Array(0.9, 0.0, 0.0, 0.0, 0.0),
				            Array(0.0, 0.1, 0.1, 0.0),
				                 Array(0.0, 0.0, 0.0),
				                      Array(0.0, 0.0),
				                           Array(0.2)
				                           			)
				                           			

		
		val clusterSanity1 = new TestNodeFactory(clusterAlone)
		//clusterSanity1.initializeFromMatrix(clusterAlone)
		
	}
	
	/**
	 * 1) Make sure that nodes themselves are behaving in a sane way.
	 * 2) Make sure that nothing is outside the range of legal probabilities.
	 * 3) Make sure that being in a cluster is better than being out of a cluster.
	 */
	@Test def clusterSanity() {
		
		val randomizer = new Random(3)
		//sequentialized for predictability with random
		
		def assertProbability(prob : Double) = {
			assert(prob <= 1.0)
			assert(prob >= 0.0)
		}
		
		val matrix1 : Array[Array[Double]] = 
			Array(Array(0.6, 0.7, 0.9, 0.1, 0.2, 0.1),
				       Array(0.1, 0.9, 0.9, 0.7, 0.1),
				            Array(0.7, 0.9, 0.6, 0.3),
				                 Array(0.4, 0.8, 0.1),
				                      Array(0.0, 0.1),
				                           Array(0.1)
				                           			)
		
		for(i <- 0 until 30){
			for(j <- 0 until matrix1.size){
				for(k <- 0 until matrix1(j).size){
					matrix1(j)(k) = randomizer.nextDouble
				}
			}
			
			val factory = new TestNodeFactory(matrix1)
			factory.nodes.values.foreach((n : Node) => {
				assertProbability(n.selfProbability)
				factory.nodes.values.filterNot(_ == n).foreach((n2 : Node) => {
					assertProbability(n.getProbability(n2))
					assertProbability(n2.getProbability(n))
				})
			})
			/*
			 * Now we take some random clusters.  We can shuffle the collection
			 * and then make a random integer in the correct range.  We take the
			 * beginning of the shuffled list up to this integer to get a random
			 * cluster.
			 */
			val shuffledNodes = randomizer.shuffle(factory.nodes.values)
			val nodeCount = (randomizer.nextDouble() * factory.nodes.size).toInt
			val c = new Cluster(shuffledNodes.take(nodeCount))
			factory.nodes.values.foreach((n : Node) => {
				assertProbability(c.probabilityCW(n))
				assertProbability(c.probabilityWC(n))
			})
			assertProbability(c.clusterness)
			assertProbability(c.mutuality(c))
			val c2 = new Cluster(shuffledNodes.takeRight(nodeCount))
			assertProbability(c.mutuality(c2))
		}
		
			
	}
	
	@Test def cliqueSanity() {
		/*
		 * That was fun.  Now that we're working with valid probabilities, lets make some matrices
		 * in which certain things definitely *should* be clusters, and some other things shouldn't.
		 * 
		 * We can start by making some cliques and then go to less obvious cases.
		 */
		
		val cliqueObvious : Array[Array[Double]] = 
			Array(Array(1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
				       Array(1.0, 1.0, 0.0, 0.0, 0.0),
				            Array(1.0, 0.0, 0.0, 0.0),
				                 Array(0.0, 0.0, 0.0),
				                      Array(0.0, 0.0),
				                           Array(0.0)
				                           			)
		for(i <- 0 until 1){
			/*
			 * TODO: Generate 10 random cliques.
			 */
			val obvF = new TestNodeFactory(cliqueObvious)
			val clustObvious = new Cluster(
					Array(obvF.nodes("n0"), obvF.nodes("n1"), obvF.nodes("n2"), obvF.nodes("n3"))
			)
			val outside = obvF.nodes.values.toSet -- clustObvious.cluster
			clustObvious.cluster.foreach((n : Node) => {
				//println(clustObvious.probabilityCW(n))
				assert(clustObvious.probabilityCW(n) > 0.99)
				assert(clustObvious.probabilityWC(n) > 0.99/clustObvious.cluster.size)
			})
			outside.foreach((n : Node) => {
				assert(clustObvious.probabilityCW(n) < 0.01)
				assert(clustObvious.probabilityWC(n) < 0.01)
			})
			//println(clustObvious.clusterness)
			assert(clustObvious.clusterness > 0.99)
			assert(clustObvious.mutuality(clustObvious) > 0.99)
			
			/*
			 * Now that we've tested that the clique behaves like a clique, we should test that
			 * subcliques have reasonable values.
			 * 
			 */
			Range(1, clustObvious.cluster.size).foreach((i : Int) => {
				val subCluster = new Cluster(clustObvious.cluster.take(i))
				//TODO: figure out what subcliques should return
				assert(subCluster.mutuality(clustObvious) > 0)
			})
		}
		
		/*
		 * If we've done all possible tests with an obvious cluster, what next?
		 */
	}

}

















