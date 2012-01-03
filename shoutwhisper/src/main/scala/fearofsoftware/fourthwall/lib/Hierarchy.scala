package fearofsoftware.fourthwall.lib


import fearofsoftware.fourthwall.model._
import java.util.Date
/**
 * Holds a hierarchical clustering of items.
 * 
 * We forbid this class from rejecting new items.  This way, the flow
 * keeps on moving.  We will allow it to keep many, many more items
 * than would normally be allowed in the UI.  This also makes keeping
 * history trivial.
 * 
 * This class is synchronized so that it can be passed around through
 * comet clients and modified concurrently.  Clusters are immutable, so
 * once one is found, we can feel free to use it for parents.
 * 
 * Uses an adaptation of http://en.wikipedia.org/wiki/Single-linkage_clustering
 * 
 * Being agglomerative makes this algorithm somewhat unstable - a small change in
 * the beginning can cascade through and re-configure the entire tree.
 * 
 */
class Hierarchy(relevance : DiscourseObject => Double){
	
	/**
	 * This dictates the size of an array of bytes.  The RAM usage of this should usually
	 * be insignificant compared to the RAM usage of keeping all those clusters in memory.
	 */
	private def MAX_CLUSTERS = 500
	
	/**
	 * Array holds the cluster created at each level of the agglomeration.  Does not
	 * hold leaves.
	 */
	private val clusters = new Array[Cluster](MAX_CLUSTERS)
	
	/**
	 * Keeps track of cluster levels.
	 */
	//private val levels = scala.collection.mutable.Map[Cluster, Int]()
	private val leaves = scala.collection.mutable.Set[Cluster]()
	private var count = 0
	private val inOut = new scala.collection.mutable.Queue[DiscourseObject]()
	clear
	
	private def compareDiscourse(d1 : DiscourseObject, d2 : DiscourseObject) : Double = {
		d1.wordBagCompare(d2)
	}

	/**
	 * Assumptions and properties of the hiearchy:
	 * When a new node comes along, we essentially perform a greedy tree search to see which
	 * cluster we should add it to.
	 * If it adds to a low cluster, that addition will propagate up the tree, such that parents
	 * have that node as well.  This is how we emulate the effect of merging 2 clusters.
	 * This may have changed the extent to which those clusters would have been able to perform
	 * further merges.  We can attempt to correct for this by re-applying that stage of the algorithm
	 * if it fails to check out, and then rebuilding from that level if necessary.
	 * 
	 * Because we know what merge was conducted at each step of the algorithm, we can start from the
	 * bottom and check whether the new node would have blocked that merge from happening.  If so, we
	 * take the new merge and then go up a level, seeing if the next level would be blocked.
	 * 
	 * If we do this post-pruning, we can potentially ensure full consistency with the original
	 */
	class Cluster(val leaves : Set[DiscourseObject], val parents : Set[Cluster]){
		
		def this(par : Set[Cluster]) = this(par.flatMap(_.leaves), par)
		
		def size = leaves.size
		
		def mutuality(other : Cluster) : Double = {
			other.leaves.map(mutuality(_)).sum/other.size
		}
		
		def mutuality(d : DiscourseObject) : Double = {
			leaves.map(compareDiscourse(d, _)).sum/this.leaves.size
		}
		
		override def hashCode = {
			leaves.hashCode ^ parents.hashCode
		}
		
		override def equals(other : Any) = {
			other match{
				case o : Cluster => {
					(o.leaves == leaves) && (o.parents == parents)
				}
				case _ => false
			}
		}
		
		/**
		 * Choose num representatives for this cluster to place in the UI.
		 * 
		 * TODO: update this code to grab the thing with most INFORMATION, probably meaning uniqueness
		 * from other clusters as well as centrality and/or relevance.
		 */
		def representatives(num : Int) : List[DiscourseObject] = {
			num match{
				case 1 => {
					val d = leaves.map(leaf => (leaf, mutuality(leaf) * relevance(leaf))).reduceLeft(
						(acc : (DiscourseObject, Double), x : (DiscourseObject, Double)) => {
							if(acc._2 > x._2) acc else x
						})
					List[DiscourseObject](d._1)
				}
				case 2 => {
					//then let's choose those furthest apart in time
					val semiSorted = leaves.toList.sort((x : DiscourseObject, y : DiscourseObject) => {
						(x.get[Date]("date") compareTo y.get[Date]("date")) > 0
					})
					List[DiscourseObject](semiSorted.first, semiSorted.last)
				}
				case _ => {
					leaves.map(leaf => (leaf, mutuality(leaf))).toList.
						sort((x : (DiscourseObject, Double), y : (DiscourseObject, Double)) => 
					x._2 > y._2).take(num).map(_._1)
				}
			}
			
		}
		
		def merge(other : Cluster) = {
			new Cluster(Set[Cluster](other, this))
		}
		
		/**
		 * What if this node does not have parents?  If it's a leaf, then what do
		 * we do with it?
		 */
		def parentMutuality = {
			parents.first.mutuality(parents.tail.first)
		}
	}
	
	def addLeaf(d : DiscourseObject) = {
		addMany(List[DiscourseObject](d))
	}
	
	def addMany(ds : List[DiscourseObject]) = {
		alter(ds, List[DiscourseObject]())
	}
	
	/**
	 * Going back down the hierachy is not that different from
	 * building it up in the 1st place.
	 * 
	 * How the hell do we go back down an agglomeration tree?
	 * Moving down the array will fail, because we might miss clusters that were
	 * merged in very, very late but 
	 * 
	 * What if we go down the array until we know that sufficient merges have been
	 * undone, and then look at everything that's still in play?  So we use the
	 * array to effectively unravel the previous steps.  We can guarantee that the
	 * previous step in the array did create a cluster that is somewhere in the
	 * set of what we have expanded, because we know that the top of the tree
	 * is all-encompassing.  So when we go down, we are looking for a cluster
	 * we have already seen, to unexpand it.
	 * 
	 */
	def getRepresentatives(num : Int) : List[Cluster] = {
		synchronized{
			if(count <= num){
				return leaves.toList
			}
			val found = scala.collection.mutable.Set[Cluster]()
			found += clusters(count - 1)
			for(i <- 1 until (num+1)){
				val levelCluster = clusters(count - i)
				found -= levelCluster
				found ++= levelCluster.parents
			}
			found.toList
		}
	}
	
	def clear = {
		synchronized{
			Range(0, clusters.size).foreach(clusters(_) = null)
			leaves.clear
			inOut.clear
			count = 0
		}
	}
	
	/**
	 * When adding a leaf, we traverse the levels and see at each whether the merge
	 * done then was still valid.
	 * 
	 * To add an element:
	 * 
	 * If we find that the level was valid, we go up to the next.
	 * 
	 * If we find that the last level was invalid, we replace it with the new cluster.
	 * Now we must check the next level to see if it was changed because of the new
	 * cluster's existence and/or the old cluster's non-existence.  We can at this
	 * point remove the original node.
	 * 
	 * Can we do this in a single pass for multiple clusters?  We'd have to check
	 * each level for if:
	 * 1) Did the new cluster(s) override the merge?
	 * 2) Were any removed cluster(s) involved in the merge?
	 * 
	 * Either way, we should be able to update the maps.
	 * 
	 * When adding clusters, we will end up with excess clusters in the system.
	 * We may have to keep a set of the clusters currently in play at each level.
	 * 
	 * Inplay starts with all the leaves and then shrinks continually until we reach
	 * the end point.  This seems to be correct, because it will shrink by count, which
	 * is its size.
	 * 
	 * It is correct that at each level, we create 1 new cluster.  Therefore, the cluster
	 * array should hold something of value - just need to know what.
	 * 
	 * The last entry should be the top cluster.  The next 2 entries should be its parents.
	 * The next 2 are whatever clusters got created before them.
	 * 
	 * Note that count should not be exactly equal to the number of leaves in play.  If we
	 * have 2 leaves, then we need 1 cluster to combine them.  If we have 1 leaf, we just use
	 * that.  If we have 3 leaves, we should be able to commit 2 merge operations.  So count
	 * should be one less than the number of leaves.  4 leaves would require 3 merge operations,
	 * etc.
	 * 
	 */
	def alter(add : List[DiscourseObject], rem : List[DiscourseObject]) : Unit = {
		var removed = rem
		var added = add
		if(added.isEmpty && removed.isEmpty){
			return
		}
		//1st, let's see if we need to remove more
		synchronized{
			if(added.size + leaves.size - rem.size > MAX_CLUSTERS){
				//println("Space exhausted.")
				while(added.size + leaves.size - rem.size > MAX_CLUSTERS){
					if(!inOut.isEmpty){
						removed = removed :+ inOut.dequeue
					}else{
						added = added.tail
					}
				}
			}
			//TODO: make sure this is a sequential foreach
			added.foreach(d => inOut.enqueue(d))
			//now we make the sets of leaves to add and remove
			def makeCluster(d : DiscourseObject) = new Cluster(Set[DiscourseObject](d), Set[Cluster]())

			//keeps track of added clusters for fast "could this be better?" tests
			val a = scala.collection.mutable.Set[Cluster]()
			a ++= added.map(d => makeCluster(d))
			
			//this tracks removed clusters for fast "are these parents still valid?" tests
			//val r = scala.collection.mutable.Set[Cluster]()
			//r ++= removed.map(d => makeCluster(d))
			
			leaves ++= a
			leaves --= removed.map(d => makeCluster(d))
			
			/**
			 * Tracks the clusters currently available at the given level.  We start with the new
			 * leaves available to us.
			 */
			val inPlay = leaves.clone
			
			//we adjust count to deal with the correct # of clusters, possibly clearing the array
			//count should be equal to the number of leaves available - 1 (for the 0th level)
			val newSize = inPlay.size - 1
			if(count > newSize){
				//nullify clusters that are simply to be cut
				Range(newSize, count).foreach(i => {
					clusters(i) = null
				})
			}
			
			count = newSize
			
			for(i <- 0 until count){
				
				var levelCluster = clusters(i)
				
				def optimalCluster(c1 : Collection[Cluster], c2 : Collection[Cluster]) = {
					c1.flatMap(x => c2.filterNot(x == _).map(y => (x, y, x.mutuality(y)))).reduceLeft((acc : (Cluster, Cluster, Double), x : (Cluster, Cluster, Double)) =>
								if(x._3 > acc._3) x else acc)
				}
				if(levelCluster != null){
					
					def replace(parents : (Cluster, Cluster, Double)) = {
						val nc = new Cluster(Set[Cluster](parents._1, parents._2))
						//the new level should be equal to i+1 - if count is 0, cluster has level 1
						a += nc
						clusters(i) = nc
						levelCluster = nc
					}
					
					/*
					 * 1st, make sure that it wasn't derived from non-existent parentage.  If so,
					 * we run through the replace.  We want to check all of the clusters currently
					 * in play for the best new option
					 * 
					 * Not only could parents have been explicitly removed, they might also have
					 * been used up by now.
					 */
					if(!levelCluster.parents.forall(inPlay.contains(_))){
						replace(optimalCluster(inPlay, inPlay))
					}else{
						/*
						 * Now, if anything would be better than the cluster we just had, we replace it.
						 * This is not necessary to check again if we've already replaced.
						 */
						if(!a.isEmpty){
							//we only want to check new clusters that are in play
							//checking old clusters is a performance hit
							//checking new clusters that have been removed is an error
							//if a pair was in play before, then it should still be in play unless removed
							//and that way, it will already be here
							//this optimization only really helps when the new item would be added late
							val newCandidate = optimalCluster(a & inPlay, inPlay)
							
							if(newCandidate._3 > levelCluster.parentMutuality){
								//then we need to replace
								replace(newCandidate)
							}
						}
					}
				}else{
					/*
					 * Then we're past the rage of where the array previously went.  At this point,
					 * we want to just keep merging things that were in play until the end.
					 */
					val parents = optimalCluster(inPlay, inPlay)
					levelCluster = new Cluster(Set[Cluster](parents._1, parents._2))
					clusters(i) = levelCluster
					a += levelCluster
				}
				
				//# of clusters in play should never go up.
				//println("in play " + inPlay.size)
				inPlay += levelCluster
				inPlay --= levelCluster.parents
				
				//println("removing " + levelCluster.parents)
				//println("after changeup " + inPlay.size)
			}
			if(!(inPlay.size == 1)) throw new IllegalStateException("No all-encompassing cluster " + inPlay.size)
		}
		//println("Successfully added to hierarchy: " + c)
	}
}

















