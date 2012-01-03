package fearofsoftware.bagofconcepts.lib

import scala.math.sqrt

/**
 * Responsible for finding agent clusters by an article.
 */
class ArticleMatcher(store : MapClusterStore, nf : NodeFactory){
	
	
	/**
	 * Takes a message with all its various attributes.  This class
	 * implements the DP algorithm for finding cluster convergence.
	 * 
	 * 
	 */
	def find(message : ConceptMessage) = {
		val text = message.getText

		//TODO: figure out a better way to split into words
		//we may need phrases and similar
		val words = text.split("[^a-zA-Z0-9]*")

		//if we make this parallel, then just make the map concurrent
		val wordBag = new scala.collection.mutable.HashMap[Node, Int]()
		words.foreach((s : String) => {
			val n = nf.findForWord(s)
			if(wordBag.contains(n)){
				wordBag.put(n, wordBag(n) + 1)
			}else{
				wordBag.put(n, 1)
			}
		})
		
		val clusters = wordBag.keys.flatMap((n : Node) => {
			store.clustersByNode(n)
		}).toSet
		
		/**
		 * Measure of how "explainable" the words in this article are by the clustering
		 * given.  Relies on prior explanations and the article's words.
		 * 
		 * We have 2 things to consider:
		 * 1) How explainable the article is in the list of topics.
		 * -flaw: may favor introducing excess topics.
		 * 2) How explainable the topics are given this article.
		 * -flaw: may fail to explain parts of the article.
		 * 
		 * Would also be nice if this would fit in with a good way to figure out the
		 * extent to which an article is about a given topic.
		 * 
		 * Note that our "cluster cover" metric is a mirror of the "representative cover"
		 * metric from the grid state.  In the grid state, we have 3 words and want to know
		 * how well they cover a number of clusters, without overlap - now we want to know
		 * how well a bunch of clusters cover some words, with overlap penalized, or alternatively
		 * how well these words explain these clusters, without leaving any words hanging.
		 * 
		 * We can attempt a comparison against clusternesses.  This will penalize highly cohesive
		 * clusters more.
		 * 
		 * We could use a constant, which would be dangerous if our data change in magnitude anytime soon.
		 * 
		 * A cluster with many slightly better explanabilities should still push out a slightly worse
		 * cluster, but one with few might become extraneous if it does not help explain some other words
		 * as well as the cluster it would push out.
		 * 
		 * What if we took an average self-explanability of the article?  Maybe this would then become
		 * the benchmark for new topics.
		 */
		def expl(cover : Set[Cluster]) = {
			
			//TODO: check this measure for sanity
			val benchmark = sqrt(wordBag.keys.foldLeft(0.0)((a1 : Double, n1 : Node) => {
				a1 + wordBag.keys.foldLeft(0.0)((a2 : Double, n2 : Node) =>{
					a2 + n1.getProbability(n2)
				})
			}))
			/*
			 * For every word in the article, we find the best explaining cover.
			 * There does not appear to be any better way to do this than to
			 * iterate the clusters for that word.
			 * 
			 * We use p(c|w), because we are interested in how well the cluster explains
			 * the word, not necessarily how well it predicts it.  We don't care if the
			 * cluster has many words, but do we care of the world has many clusters?
			 * If the word does, it will score low here and may have a higher self probability
			 * than cluster probability - which also sounds about right.
			 */
			val explanationTotals = wordBag.foldLeft(0.0)((acc : Double, entry : (Node, Int)) => {
				val node = entry._1
				val freq = entry._2
				
				val wordExpl = (node.selfProbability ::
						store.clustersByNode(node).filter(cover.contains(_)).map((c : Cluster) => {
					c.probabilityCW(node)
				}).toList).max
				
				acc + wordExpl
			})
			/*
			 * Now we need to penalize extraneous clusters, even if they are adding a little bit.
			 * We require the following properties:
			 * -if a word is reasonably explainable in fewer clusters, favor fewer
			 * -if a word has no reasonable explanation w/out a cluster, make sure it's in
			 * 
			 * So we have to define what counts as "reasonably explained" in terms of a number, and
			 * then set the cluster inclusion penalty around that number.
			 * 
			 * Since we have a provided list of listening clusters, the standard for what is reasonable
			 * depends on other parts of the system.
			 * 
			 */
			explanationTotals/(benchmark)
		}
		
		/*
		 * Map of cluster covers.
		 * 
		 * 
		 * Flaws:
		 * -fails to account for double-entendre
		 * Is this auto-mitigated by the likely presence of words for both meaning
		 * clusters of the problem word?
		 */
		var clusterings : List[Map[Set[Cluster], Double]]
				= clusters.map((c : Cluster) => {
					val sc = Set[Cluster](c)
					(sc, expl(sc))
				}).toMap :: Nil
		/*
		 * Use dynamic programming to build up a clustering based on explainability.
		 * We want the smallest reasonable cover of clusters.
		 * 
		 * We start by taking single clusters.  Each of these will probably fit the article
		 * relatively poorly.  We then expand each level of clusters by one, until we have
		 * found an optimal set of clusters for which to search.
		 * 
		 * We avoid expanding whenever adding the new cluster would reduce the overall rating
		 * of the cover.
		 * 
		 */
		for(i <- 1 until words.length){
			val bigger = clusterings.first.flatMap((entry : (Set[Cluster], Double)) => {
				clusters.filterNot(entry._1.contains(_)).map((c : Cluster) => {
					val nc = entry._1 + c
					(nc, expl(nc))
				}).filter(_._2 >= entry._2)
			}).toMap
			
			clusterings = bigger :: clusterings
		}
		
		val clustMap = clusterings.flatten
		
		clustMap.reduceLeft((e1 : (Set[Cluster], Double), e2 : (Set[Cluster], Double)) => {
			if((e1._2) > e2._2) e1 else e2
		})
	}
}















