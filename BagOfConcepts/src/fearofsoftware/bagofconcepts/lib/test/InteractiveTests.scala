package fearofsoftware.bagofconcepts.lib.test

import java.util.Scanner

import fearofsoftware.bagofconcepts.lib._
import scala.util.Random

/**
 * Should hold code for running common functions on the command line.
 */
object InteractiveTests {

	def main(args:Array[String]) = {
		//testClusternessStuff
		test2ClustersLargeGrid
	}
	
	/**
	 * Trying to debug:
	 * -slight subclusters do significantly better than merged clusters.  Is this acceptable at all?
	 * It might be wholly acceptable.
	 */
	def testClusternessStuff = {
		val array : Array[Array[Double]]=
			Range(0,100).map((i : Int) => {
				Range(0,100 - i).map((j : Int) => {0.0}).toArray[Double]
			}).toArray;
		

		fuzzArrays(array)
		clusterArraySection(0, 21, array)
		clusterArraySection(50, 70, array)
		
		Range(50, 70).foreach((i : Int) =>{
			array(0)(i - 1) = 0.99999
		})
		
		val nf = new TestNodeFactory(array)
		
		val c1 = new Cluster(Range(0, 21).map((i : Int) => nf.findForWord("n" + i)).toSet)
		val c2 = new Cluster((0 :: Range(50, 70).toList).map((i : Int) => nf.findForWord("n" + i)).toSet)
		val ct = c1.merge(c2)
		println(ct)
		println(c1)
		println(c2)
		println("total cluster: " + ct.clusterness)
		println("subcluster 1: " + c1.clusterness)
		println("subcluster 2: " + c2.clusterness)
		val crummier = new Cluster(Range(0,20).map((i : Int) => nf.findForWord("n" + i)).toSet)
		println("---")
		println("crummy c1: " + crummier.clusterness)
	}
	
	private def clusterArraySection(start : Int, end : Int, generatedArrays : Array[Array[Double]]) = {
		Range(start, end - 1).foreach((i : Int) => {
			Range(0, end - i - 1).foreach((j : Int) => {
				generatedArrays(i)(j) = 0.99999
			})
		})
	}

	private def fuzzArrays(generatedArrays : Array[Array[Double]]) = {
		val rand = new Random(5)
		Range(0,generatedArrays.size).foreach((i : Int) => {
			Range(0, generatedArrays(i).size).foreach((j : Int) => {
				if(generatedArrays(i)(j) == 0.0){
					generatedArrays(i)(j) = rand.nextDouble*0.1
				}
			})
		})
	}
	
	def printValues(array : Array[Array[Double]]) = {
		println(
			Range(0,array.size).foldLeft("")((acc : String, i : Int) => {
				acc + (" " * 5 * i) +
				Range(0, array(i).size).foldLeft("")((acc : String, j : Int) => {
					acc + "(%01.1f)".format(array(i)(j))
				}) + "\n"
			})
		)
	}
	
	//-XX:+UseNUMA
	private def testWithGridInput(array : Array[Array[Double]]) = {
		val nf = new TestNodeFactory(array)
		val state = new GridState(nf)
		
		val sc = new Scanner(System.in)
		//println("Enter the 1st word to begin.")
		sc.next()
		state.update("n0")
		while(state.aboutToDecide != null){
			println("Options:")
			val optionList = state.aboutToDecide.toList
			for(i <- 0 until optionList.size){
				println("\t" + i + ")" + optionList(i))
			}
			val lineChoice : Int = sc.nextInt()
			if(lineChoice >= 0 ){
				state.update(optionList(lineChoice))
			}else{
				state.update(null)
			}
		}
		println("Cluster selected: " + state.chosenCluster)
	}

	def test2ClustersLargeGrid = {
		/*
		 * First, we need to programatically generate a very large matrix.
		 * We want a small cluster in one place and another in some other place.
		 * We can simply generate collections that will create the desired arrays.
		 */
		val array : Array[Array[Double]]=
			Range(0,100).map((i : Int) => {
				Range(0,100 - i).map((j : Int) => {0.0}).toArray[Double]
			}).toArray;
		

		fuzzArrays(array)
		clusterArraySection(0, 21, array)
		clusterArraySection(50, 70, array)
		
		Range(50, 70).foreach((i : Int) =>{
			array(0)(i - 1) = 0.99999
		})
		
		//printValues(generatedArrays)
		
		testWithGridInput(array)
	}
	
	/**
	 * Runs an interactive test on the grid, taking user input and attempting to converge on a cluster.
	 */
	def test2clustersInteractive = {
		//branching clusters
		val clusterBranch : Array[Array[Double]] =
			Array(Array(0.9, 0.8, 0.0, 0.1, 0.2, 0.8),
				       Array(0.9, 0.0, 0.0, 0.0, 0.0),
				            Array(0.0, 0.1, 0.1, 0.0),
				                 Array(0.7, 0.0, 0.8),
				                      Array(0.0, 0.9),
				                           Array(0.0)
				                           			)
		
		testWithGridInput(clusterBranch)
	}
	
}