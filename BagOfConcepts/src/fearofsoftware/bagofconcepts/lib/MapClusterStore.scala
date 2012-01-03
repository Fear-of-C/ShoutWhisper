package fearofsoftware.bagofconcepts.lib

import scala.collection.JavaConversions.JConcurrentMapWrapper

/**
 * Stores clusters using maps.  Could be swapped out for a database-based store.
 */
class MapClusterStore {

	private val clusters : scala.collection.mutable.Map[Node, Set[Cluster]] = 
			new JConcurrentMapWrapper(new java.util.concurrent.ConcurrentHashMap[Node, Set[Cluster]]())
	
	def addCluster(c : Cluster) = {
		c.cluster.foreach((n : Node) =>{
			if(!clusters.contains(n)){
				clusters.put(n, Set[Cluster](c))
			}else{
				clusters.put(n, clusters(n) + c)
			}
		})
	}
	
	def clustersByNode(node : Node) : Set[Cluster] = {
		Set[Cluster]()
	}
}