package fearofsoftware.bagofconcepts.lib

import scala.collection.immutable.Set
import scala.math.sqrt

/**
 * 
 * We almost without a doubt want a caching system here.  This means that we need some
 * place to store past clusters with a fast search system to see if a cluster is
 * already in the table.
 * 
 * We could potentially allow a sortability on nodes, assuming that no 2 nodes should
 * have the same string.  This would allow us to find pieces of clusters using a suffix
 * table.  It may not, however, be much of an improvement in real use cases.  We still
 * would not know which subsets of nodes would already be in clusters without some
 * rather intensive computations.
 * 
 * So we will use a hashtable of cluster sets to computed cluster.  This cannot guard
 * against re-creating the same sets, but it can remove the need to re-do expensive
 * computations and save memory by preventing multiple copies of a cluster from
 * existing.  We will encourage code that uses clusters to use the append and subtract
 * methods when possible to avoid duplication of effort.  Hopefully we do not have
 * anywhere that we build many clusters out of the blue.
 * 
 * For now, it may even be enough to simply keep these computation results on single
 * clusters - we use 8 more doubles per clusters but avoid large, iterative computations.
 * In the future, we may have different types of cluster for different sizes.  At the very
 * least, we shave about a factor of cluster.size off the DP algorithm.
 */
class Cluster(c : Collection[Node], ct : Double, et : Double){
	
	def neighborThreshold = 0.1
	val cluster : scala.collection.immutable.Set[Node] = c.toSet
	
	def this(c : Collection[Node]) = this(c, -1.0, -1.0)
	def this(n : Node) = this(Set[Node](n))
	
	//val parent = p
	
	/*
	 * Cached variables.  For robustness against very large datasets.
	 */
	val clusterTotal : Double = if(ct >= 0.0) ct else computeClusterTotal
	val exteriorTotal : Double = if(et >= 0.0) et else computeExteriorTotal
	
	/**
	 * The total sum of cluster-internal links.  It is like the unnormalized version of
	 * p(W|C) for every word that is already in this cluster.
	 * 
	 * We want to make sure that we count links both ways (in case they are different).
	 * This means that every node has a bidirectional (x2) link to itself, which we must
	 * watch out for in other computations.  It basically means that we max out at
	 * 2 * (cluster.size^2).
	 */
	def computeClusterTotal = {
		/*val c = cluster.toArray
		Range(0, c.size).foldLeft(0.0)((acc : Double, i : Int) => {
			acc + Range(i, c.size).foldLeft(0.0)((acc : Double, j : Int)=> {
				acc +  c(i).getProbability(c(j))
			})
		})*/
		cluster.foldLeft(0.0)((acc : Double, n1 : Node) => {
			acc + cluster.foldLeft(0.0)((acc : Double, n2 : Node) => {
				acc + n1.getProbability(n2)
			})
		})
	}

	/**
	 * Total links leading out of the cluster.
	 */
	def computeExteriorTotal = {
		cluster.foldLeft(0.0)((acc : Double, point : Node) => {
			acc + point.getNeighbors(neighborThreshold).filterNot(cluster.contains(_)).
			foldLeft(0.0)(_ + point.getProbability(_))
		})
	}

	/**
	 * All the links from things in the cluster.  If the cluster is 1 word, this matches
	 * the value for the word.
	 */
	def linkTotal = {
		clusterTotal + exteriorTotal
	}

	/**
	 * Should be something like the probability that each word is actually in
	 * this cluster.  There are, however, some shortcuts for this approach.
	 * 
	 * This measure should return 1.0 on a clique with no links to the
	 * outside.
	 * 
	 * How much should 2 cliques need to be linked in order to have greater
	 * mutuality than either one?  Linking them will subtract from the top
	 * term of cliqueness but add to the top term of internality.
	 * 
	 * Under what circumstances would we want a bunch of words to be called
	 * a single cluster?
	 * -If 2 cliques joined by 1 node, almost certainly no.
	 * -If about half the words in each, would be ambiguous.  What about 1/10?
	 * Given that word usage probably follows power laws, a lot of words from
	 * the same cluster might not be linked as well.
	 * 
	 * -The difference should probably less than the penalty for taking any
	 * subclique.  So if we have 2 cliques that join by one node, having
	 * both cliques as one or the 2 cliques individually should still be better
	 * than having either option with one of the cliqued nodes removed.  This
	 * gives us a benchmark for more function properties.
	 * 
	 * Things we know so far:
	 * -linkTotal > cluster.size^2 if we're looking at a clique with external
	 * connections, equal for a clique, and potentially less for a non-clique.
	 * -The inverse relationship with size squared will start to favor smaller
	 * clusters as soon as 
	 * 
	 * When we merge 2 cliques of size n with 1 fully connected node:
	 * -clusterTotal should double and then add n.  It was previously proportional
	 * to (n^2).  Since the top is quadratic in it, it gains a factor of 4.
	 * -Size doubles, causing the bottom term in cliqueness to quadruple.
	 * -The bottom should also gain a factor of 2+n through linkTotal.
	 * So now we have the bottom going up by 8X and the top by 4X.  What gives?
	 */
	def clusterness : Double = {
		if(cluster.size == 0) return 1.0
		val r = cliqueness * internality
		return r
	}
	
	/**
	 * What % of the linkage for a clique does this cluster have?
	 * Properties:
	 * -If links are on average less than 1.0, then this method favors
	 * smaller clusters by a roughly constant factor.  Let's say that link
	 * average is 0.5.  Then adding a new node, we get 0.5*size on top,
	 * and a factor of (size + 1) on the bottom.  We could in theory
	 * multiply the bottom by a factor of the average linkedness of
	 * clusters in the graph in order to even this out.
	 * -In a fully connected graph, this method will return 1.0 always. 
	 */
	def cliqueness : Double = {
		(clusterTotal)/(2 * cluster.size * cluster.size)
	}
	
	/**
	 * -Independent of the average linkedness of clusters, assuming that
	 * external links share that same average.
	 * -May favor larger clusters.  If we assume a random graph, then
	 * the area "enclosed" by a cluster is quadratic in its size, while
	 * the area "external" to it is linear but multiplied by a huge constant.
	 */
	def internality : Double = {
		clusterTotal/linkTotal
	}

	/**
	 * Whether this contains the other cluster in its entirety.
	 */
	def supercedes(other : Cluster) = {
		(other.cluster & cluster).size == other.cluster.size
	}

	/**
	 * This takes the amount of weight going to things in the cluster and divides
	 * by the total amount of links coming from the word.  It does not yet
	 * account for centrality, as doing so would require some renormalizing in
	 * order to ensure that it does not go above 1.0
	 * 
	 * If words inside the cluster were weighted by centrality, we could then
	 * take 
	 * 
	 * Currently requires no bounding.
	 * 
	 * If this cluster is a clique with no external links, then this method
	 * should return 1.0 for all contained nodes and 0.0 for all external
	 * nodes.
	 */
	def probabilityCW(point : Node) : Double = {
		if(cluster.size == 0) return 0.0
		val r = cluster.foldLeft(0.0)((acc : Double, to : Node) => {
			acc + point.getProbability(to)
		})/point.getLinkTotal
		if(r.isInfinite) return 1.0
		return r
	}
	
	/**
	 * Given that this cluster was chosen, what is the probability that we will find item?
	 * -We need to take into account the clusterness, as stronger clusters are more likely
	 * to point inward and less likely towards an external node.  If item is inside the cluster,
	 * we still must weight it against the other nodes in the cluster.
	 * -We should take into account the exterior.  If a cluster links heavily to all nodes
	 * everywhere and happens to link to item, we should not necessarily upweight the
	 * probability of item.
	 * -We may not consider the item's total links, because then we are impinging upon
	 * the territory of probabilityCW.  We do want these to have distinct meanings that
	 * actually refer to probabilities.
	 * 
	 * Note that this disfavors larger clusters.  That makes sense, because there is more
	 * chance that taking a random link from a large cluster will result in some other word.
	 * This is not necessarily the probability that the word should be in the cluster
	 * (which again would impinge on probabilityCW).
	 * TODO: re-evaluate this interpretation
	 * 
	 * If we would get an infinite number, then we are probably dealing with a cluster
	 * that has no internal links, which can happen.  Return 1.0, because a cluster with
	 * no internal links might as well include anything.
	 * 
	 * If the cluster's size is 0, then return 0.0, since the empty cluster is perfectly
	 * efficient and contains nothing.
	 * 
	 * If this cluster is a clique with no external links, then this method should return
	 * 0.0 for all nodes outside of the cluster and about 1/size for all nodes inside.
	 */
	def probabilityWC(item : Node) : Double = {
		if(cluster.size == 0) return 0.0
		//boundProbability(probabilityCW(item) * item.getLinkTotal / clusterness)
		val r = cluster.foldLeft(0.0)((acc : Double, to : Node) => {
			acc + item.getProbability(to)
		})/linkTotal
		if(r.isInfinite) return 1.0
		return r
	}
	
	/**
	 * Average amount by which a random node in this cluster would linke to item.
	 * Could late be updated with centrality.
	 */
	def averageLinkage(item : Node) : Double = {
		if(cluster.size == 0) return 0.0
		if(item == null) return 1.0
		val r = cluster.foldLeft(0.0)((acc : Double, to : Node) => {
			acc + item.getProbability(to)
		})/cluster.size
		if(r.isInfinite) return 1.0
		return r
	}

	/**
	 * Returns the nodes 1 link away from this cluster.
	 */
	def getFringe(threshold : Double) = {
		cluster.foldLeft(Set[Node]())((acc : Set[Node], node : Node) => {
			acc ++ node.getNeighbors(threshold).filterNot(cluster.contains(_))
		})
	}

	override def hashCode : Int = {
		cluster.hashCode
	}
	
	override def equals(that : Any) = that match{
		case other : Cluster => cluster.equals(other.cluster)
		case _ => false
	}

	def append(node : Node) = {
		if(cluster.contains(node)){
			this
		}else{
			val newNeighbors = (node.getNeighbors(neighborThreshold)- node).filterNot(cluster.contains(_))
			new Cluster(cluster + node,
					clusterTotal + cluster.foldLeft(0.0)(_ + _.getProbability(node)),
					exteriorTotal + cluster.foldLeft(0.0)((acc : Double, n1 : Node) => {
						acc + newNeighbors.foldLeft(0.0)((acc : Double, n2 : Node) => {
							acc + n1.getProbability(n2)
						})
					}))
		}
	}

	//TODO: save computations here.
	def subtract(node : Node) = {
		new Cluster(cluster - node)
	}

	def merge(other : Cluster) = {
		new Cluster(other.cluster ++ cluster)
	}

	/**
	 * This method is now clusterness-neutral.  Instead, it measures the average
	 * connections between the two clusters.  It should return 1.0 between
	 * 2 halves of a clique or between a clique and itself.
	 * 
	 */
	def mutuality(other : Cluster) : Double = {
		if((cluster.size == 0) || (other.cluster.size == 0)) return 1.0
		val r = cluster.foldLeft(0.0)((acc : Double, node1 : Node) => {
			acc + other.cluster.foldLeft(0.0)((acc : Double, node2 : Node) => {
				acc + node1.getProbability(node2)
			})
		})/(cluster.size * other.cluster.size)
		if(r.isInfinite) return 1.0
		return r
	}

	/**
	 * Does predict combined clusterness.  When one of the clusters is very
	 * small, this should approach p(w|c).  When both of the clusters are very
	 * large, this will probably approach mutuality.
	 */
	def combinedClusterness(other : Cluster) = {
		merge(other).clusterness
	}
	
	override def toString = {
		"Cluster(" + hashCode + " " + cluster.toList.sort(_.toString < _.toString) + ")"
	}
}

/*
 * We need to hold some kind of database of clusters that agents implement.
 * We can actually recycle this in the cluster implementation to implement
 * a sort of caching/de-duplication strategy.
 * 
 * The purpose of this object is to allow all clusters created to auto-register
 * upon creation.  This way, we can use as a cache.  It may not be strictly necessary.
 * 
 */
/*
object Cluster{

	var backend : ClusterStore = null
	
	/**
	 * We want to be able to swap out what mechanism is storing clusters.  This could
	 * be a database, or a simple hashmap, or really anything.
	 */
	def setBackend(store : ClusterStore) = {
		
	}

	def clustersByNode(node : Node) : Set[Cluster] = {
		backend.clustersByNode(node)
	}
}
*/











