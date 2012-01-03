package fearofsoftware.bagofconcepts.lib

import scala.collection.immutable.Set
import scala.collection.mutable.PriorityQueue
import scala.math.log
import scala.math.pow
import scala.math.sqrt
import scala.util.Sorting.quickSort


import scala.collection.JavaConversions.JConcurrentMapWrapper
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.PriorityQueue


/**
 * Uses the notion of concepts as clusters of words, where each word has multiple concepts attached
 * to it.  Highly stateful and should be used by a single thread.
 */
class GridState(nf : NodeFactory){
	
	/**
	 * At what entropy does asking more questions stop being useful?
	 * 
	 */
	private final def entropyThreshold = 5.0
	
	private final def maxClusters = 1000
	
	private final def entropyMax = 100
	
	private final def mergeMax = 60

	var history : List[(Node, Set[Node])] = Nil

	/**
	 * Publicly available.  Calling "update" on this class will cause these
	 * to change to reflect new info.  Will be null when choice is finished.
	 */
	var aboutToDecide : Set[String] = Set()
	
	private var nodesToDecide : Set[Node] = Set()
	
	private var clusterProbabilities : List[Map[Cluster, Double]] = Nil
	
	/**
	 * Will store the conceptual cluster which the user has converged upon
	 * when aboutToDecide is null.
	 */
	var chosenCluster : Cluster = null
	
	private def probEntropy(prob : Double) = {
		- prob * log(prob)/log(2)
	}
	
	/**
	 * Convenience method for making a map correspond to probabilities usable in an entropy calculation.
	 */
	def normalized(m : Map[Cluster, Double]) = {
		val sum = m.values.sum
		m.map((entry : (Cluster, Double)) => {
			(entry._1, entry._2/sum)
		}).toMap
	}

	/**
	 * We might use a weak assumption: that the clusters provided sum to 1 in their probabilities.
	 * Then, for each node, we take distance from each cluster and force that to total to 1.  For each reject
	 * node, we downweight by closeness to chosen node (no downweight if none was chosen).
	 * Has the following sanity properties:
	 * 1) If a cluster has multiple representatives on the grid, then it should receive a net upweight if
	 * even one of those was chosen.  If no node or a distant node was chosen, then it will receive penalty
	 * for all of them.
	 * 2) If a cluster has no representatives on the grid, the choice should not affect it.
	 * 
	 * This means we assume that we're choosing something within our clustering, not necessarily something
	 * within the grid.  It skips the "grid probability" question for the most part.
	 * 
	 * Pathological situations:
	 * 1) The user selects a node that is very close to many clusters.  Impact is diluted from what it would be
	 * if there had been only 1 nearby cluster (is this even a flaw?)  Similarly for rejections.
	 * 
	 * Do we want to take into account the prior probability of the word?  We do implicitly if we assume that
	 * the user is definitely looking for a cluster currently on the grid.
	 * 
	 * -Any choice that does not contain the base cluster gets filtered out.
	 * -If a cluster is fully linked to the chosen node, it stays pretty much the same on that part.
	 * -If a cluster is fully linked to a rejected node that is not close to an accepted node, then
	 * it should fall very quickly - the # that goes in will be almost 0.
	 * -If a cluster is not linked to a rejected node, then it doesn't care.
	 * -If a cluster is not linked to the accepted node, then it does care strongly about it.
	 * -If a cluster is linked strongly to a rejected node, but that node is strongly linked to
	 * the accepted node, then it should care a little but not much.  If it's not linked to
	 * the accepted node, then it is thoroughly screwed anyway.
	 * 
	 * Why do things not converge quickly then?  How much does it take for entropy to fall below 1.0?
	 * Maybe a ton more than we think.
	 * 
	 * Part of the problem is that we have trouble distinguishing between multiple clusters that all
	 * contain the choice node and are equivalent from the grid's perspective.  In theory, this grid
	 * should not appear unless there are sufficient eliminable clusters to kill.
	 * 
	 */
	private def recomputeProbabilities(s : Collection[Cluster]) : Map[Cluster, Double] = {
		//println("Recomputing with history " + history)
		val baseNodes = history.map(_._1).filterNot(_ == null).foldLeft(Set[Node]())(_ + _)
		normalized(s.filter((x) => baseNodes.subsetOf(x.cluster)).map((c : Cluster) => {
			(c, computeProbability(c))
		}).toMap.filterNot(_._2 < 0.0001))
	}
	
	/**
	 * We say that a cluster is subsumed if:
	 * 1) It links strongly to all of the nodes in the addition, without much ambiguity.
	 * Obviously, it should improve cliqueness.
	 * 2) It could not reasonably be expanded in a way that would not have those links.
	 * So the links to the addition are at least as strong as its links to anything else.
	 * 3) The cluster is stronger in general.  It should not gain more externality relative
	 * to size (though it could gain some more external links if it's an expanding subclique).
	 * This basically means we see how much externality a clique would potentially gain
	 * during its expansion and measure against it.
	 * 
	 * Let's say we call this method for n nodes.
	 * Let's say that we have a clique expanding from a small number, m, of nodes.
	 * Let's say it's in a very large clique of size N >> n,m.
	 * The externality difference is m*N - m*n = m*(N - n).
	 * The average externality difference is N - (n + m)*(N - m)/(n + m) = -m
	 * So average externality goes down by m - that's sensible, since we took m out of n.
	 * 
	 * If we are adding 1 node, how external does it need to be to raise the average?
	 * Let the new node have externality from the cluster x, and the cluster have avg ext a.
	 * Let the cluster contain n nodes.
	 * New average = (n*a + x)/(n + 1)
	 * Difference in average = a - (n*a + x)/(n + 1) = ((n*a + a) - (n*a + x))/(n+1)
	 * =(a - x)/(n + 1)
	 * So this is negative whenever x > a.  Again, the obvious answer is correct.
	 * 
	 * Cliqueness: we want a delta here as well.
	 * 
	 * To calculate these deltas, we can just use the precomputed values in the clusters.
	 * 
	 * Also note that a cluster expands all at once, so other expansions should not be affected.
	 * 
	 * Note that cliqueness can not go up from perfect.  It can, however, go up from crappy
	 * clusters.  This does not help much in determining whether a node *should* get included.
	 * 
	 */
	private def subsumed(sup : Cluster, sub : Cluster) : Boolean= {
		def errorThreshold = 0.00001
		
		val dExtern = sup.exteriorTotal/sup.cluster.size - sub.exteriorTotal/sub.cluster.size
		val dClique = sup.cliqueness - sub.cliqueness
		
		return (sup.supercedes(sub)) && (dExtern <= errorThreshold) && (dClique >= -errorThreshold)
	}
	
	/**
	 * Old BFS expansion method.
	 */
	private def expandBFS(toExpandFrom : Set[Cluster], threshold : Double) = {
		val shelf : scala.collection.mutable.Map[Cluster, Any] =
				new JConcurrentMapWrapper(new ConcurrentHashMap(maxClusters))
		var lvl = toExpandFrom
		while(shelf.size + lvl.size <= maxClusters){
			val newClusters : scala.collection.mutable.Map[Cluster, Set[Cluster]] =
				new JConcurrentMapWrapper(new ConcurrentHashMap(lvl.size * 20))
			lvl.foreach((c : Cluster) => {
				c.getFringe(threshold).foreach((n : Node) => {
					val nc = c.append(n)
					newClusters.put(nc, 
							if(newClusters.contains(nc)) (newClusters(nc) + c) else Set[Cluster](c))
				})
			})
			//we have the new items, and they are uniqued!  check subsumptions...
			newClusters.foreach((entry : (Cluster, Set[Cluster])) => {
				shelf ++= entry._2.filterNot(subsumed(entry._1, _)).map((_, null))
			})
		}
		(shelf ++ lvl).toSet
	}
	
	/**
	 * New method based on lessons from BFS and A*.
	 * 
	 * The advantage of single-expansion at each stage is that the ask will do an incredible
	 * amount of damage and itself solve most of the pruning/subsumption question.  Any
	 * alternate paths to the same large cluster that don't contain the chosen node(s)
	 * will get killed off.
	 * 
	 * Note that further expansions actually damage this effect.  If we have a, b, c, and d,
	 * and know that c is chosen, we are better off elminating ab, ad, and bd in their stage
	 * 2 forms, or even eliminating a, b, and c right off the bat.  This property relies on
	 * symmetry: all clusters that contain a certain node should be obtainable by expanding
	 * from that node.  There might be pathological cases in which this is not true, such
	 * as if the node has many disjoint neighbors that reconnect later.
	 * 
	 * We should also compare the damage from DFS to the potential damage from rote trimming.
	 * Rote trimming will kill unlikely roots, whereas DFS will kill things that may have been
	 * partially expanded alternate paths.
	 * 
	 * Using an A*-like method rather than pure BFS can help us limit where the expansion is
	 * (so that we are expanding first on likely clusters, rather than randomly on tangents).
	 * This also might also help make further expansions out from where we already are.
	 * 
	 * Actually, A* might almost always beat BFS.  The reason is that new BFS-based branches
	 * that get created will get killed regardless if neither they nor their parent contain
	 * the next choice node.  So we gain more information by expanding as close as possible
	 * to the most likely cluster(s), assuming that we don't end up depth-first.
	 * 
	 * Basically, what we want to do is to maximize entropy.  So we want to split the most
	 * probable elements first; however, we also want to minimize redundancy.  The former
	 * is mostly size-neutral, while the latter would most likely prefer to expand disparate
	 * branches first (breadth-first).
	 * 
	 * We might simultaneously expand everything with equal probability.
	 * 
	 * Note that we already have favor to smaller clusters, since they are for the most part
	 * closer to the origin node.  This makes it much more like BFS, which we will keep around
	 * for a little while anyway.
	 * 
	 */
	def expand(toExpandFrom : Collection[Cluster]) = {
		def threshold = 0.05
		def max = 2000
		def maxIterations = 400
		def queueOverload = 500
		def queueDefault = 100
		
		val cache  : scala.collection.mutable.Map[Cluster, Double] =
				new JConcurrentMapWrapper(new ConcurrentHashMap(max))
		
		/*
		 * Holds everything we will not be expanding but still might show up in the end result.
		 */
		val shelf : scala.collection.mutable.Set[Cluster] =
				scala.collection.mutable.Set[Cluster]();
		
		def cp(c : Cluster) = {
			if(!cache.contains(c))
				cache.put(c, historicalProbability(c) * pow(c.cliqueness, history.size))
			cache(c)
		}
		
		val order = new Ordering[Cluster] {
			def compare(x : Cluster, y : Cluster) = {
				cp(x) compare cp(y)
			}
		}

		val expQ = new PriorityQueue[Cluster]()(order)
		val expS = scala.collection.mutable.Set[Cluster]()
		
		expQ ++= toExpandFrom
		expS ++= toExpandFrom
		
		var i = 0
		while((!expQ.isEmpty) && (i < maxIterations)){
			println(i)
			val c = expQ.dequeue
			expS -= c
			cache -= c
			
			val newClusters = c.getFringe(threshold).map(c.append(_)).toSet
			//we have the new items.  Check subsumptions whether or not they are unique.
			if(!newClusters.forall(subsumed(_, c))){
				shelf += c
			}
			//don't re-add things that we've already seen
			val actuallyNew = newClusters.filterNot((x) => expS.contains(x) || shelf.contains(x))
			expQ ++= actuallyNew
			expS ++= actuallyNew
			
			//check if we need to prune
			if(expQ.size > queueOverload){
				//the sorting is the slowest part
				//println("Overloaded queue.  Shelf is at " + shelf.size + "iterated to" + i)
				val array = expQ.toArray
				quickSort(array)(order)
				val retained = array.take(queueDefault)
				expQ.clear
				expS.clear
				expQ ++= retained
				expS ++= retained
			}
			i += 1
		}
		
		expS ++= shelf
		expS.toList.sort(cp(_) > cp(_)).take(max)
	}
	
	/**
	 * This is not actually much worse than the old measure of mutuality with the base cluster,
	 * but it definitely makes things more comparable.
	 * 
	 * If the chosen node is highly linked to a cluster, it must actually make a difference.
	 * 
	 * We seem to have trouble upweighting and penalizing clusters.  The algorithm is making up crummy
	 * clusters and failing to converge quickly.
	 * 
	 * Goal: when a choice is made, enough variation in the probabilities should occur that the choice
	 * is not longer interesting to us.  Multiplication might simply be inadequate.
	 * 
	 * One problem is that a very large # of clusters are likely to share the choice node.  In fact, a very
	 * large # are likely to be grid-equivalent, and in this case we need to strongly favor those with
	 * higher clusterness.  In fact, it seems that our clusterness just isn't aggressive enough - it's
	 * creating a whole lot of choices.
	 * 
	 * The other question is why the same nodes keep coming up in entropy calculations.  We should check that
	 * without a doubt, entropy falls most when we are finding new nodes to check.
	 * 
	 * We may also re-introduce the measure we had of constant maximum cluster leading to a decison having been
	 * made.
	 * 
	 * We are at this point nearly killing the clusters that contain a rejected node but not one of the accepted
	 * nodes.
	 * 
	 * What could possibly make this algorithm converge faster?
	 * -Maybe we can eliminate clusters that are similar to (but worse than) other clusters.  This makes sense from
	 * a usage perspective, as we should have meaning clusters that are relatively close.  The question is how to
	 * compute when to eliminate.  It appears that similar clusters are nearby entropy-wise, which is part of the
	 * reason why they're so hard to kill.  Subsumption could wait to expand these, but how would that affect our
	 * entropy calculations?
	 * 
	 * 
	 */
	private def computeProbability(c : Cluster) = {
		pow(c.clusterness, history.size) * historicalProbability(c)
	}
	
	private def historicalProbability(c : Cluster) = {
		history.foldLeft[Double](1.0)((acc1 : Double, s: (Node, Set[Node])) => {
			s._2.foldLeft[Double](acc1)((acc2 : Double, r: Node) => {
				acc2 * (1.0 - c.averageLinkage(r)
						* (if (c.cluster.contains(r)) (1.0/pow(c.averageLinkage(r), 0.9)) else 1.0)
						* (if(s._1 != null) (1.0 - r.getProbability(s._1)) else 1.0))
			}) * c.averageLinkage(s._1) * (if (c.cluster.contains(s._1)) 1.0 else 0.0)
		})
	}
	
	private def computeEntropy(m : Map[Cluster, Double]) = {
		m.foldLeft[Double](0.0)((acc : Double, entry : (Cluster, Double)) => {
				acc + probEntropy(entry._2)
		})
	}

	/**
	 * Called when we have a new grid choice or to initialize with a word that the user has
	 * typed in.
	 * 
	 * REDESIGN
	 * 
	 * Comparabilities required:
	 * -new and old clusters.  Expansions must compare reasonably with clusters already there.
	 * -big and small.
	 * 
	 * How long does it take to do a full Bayesian recompute on assigned probabilities?
	 * 	for each cluster in our probability map:
	 * 		assume the cluster starts with even probabilty
	 * 		then multiply by every node in the base cluster
	 * 	normalize
	 * 
	 * Since normalization always leaves the ratio between different probabilities intact, it would
	 * appear that we need not do it again.  In fact, all we really have to do is to change our "mutuality"
	 * mechanic to play catchup with the normalization.
	 * 
	 * One flaw is that we should really be starting clusters at their clusterness.  This means that the origin
	 * cluster does not get penalized, because it will be normalized to 1.0.  So what's really happening is that
	 * between 2 generations, if 1 of the generations was substantially worse than another, it will not show up.
	 * We will therefore have to run recomputations over the entire set rather than updating some clusters
	 * incrementally.
	 */
	def update(accepted : String) : Unit = {
		
		/*
		 * First update the history.  This will allow us to pop things off of it later, should the user decide
		 * to undo a word choice.
		 */
		val rejectSet = (aboutToDecide - accepted).map((s : String) => nf.findForWord(s))

		val node = if(accepted != null) nf.findForWord(accepted) else null

		history ::= (if(accepted != null) node else null, rejectSet)
		
		clusterProbabilities ::= (if (clusterProbabilities == Nil) Map[Cluster, Double]((new Cluster(node :: Nil), 1.0))
			else recomputeProbabilities(clusterProbabilities.first.keys))
			
		//println("Starting with " + clusterProbabilities.first.values.take(30))
		val entropy = computeEntropy(clusterProbabilities.first)
		
		//println("entropy" + entropy)
	
		//adding new expansion code...
		clusterProbabilities = recomputeProbabilities(expand(clusterProbabilities.first.keys)) :: clusterProbabilities.tail
		val maxCluster = clusterProbabilities.first.reduceLeft[(Cluster, Double)]((x : (Cluster, Double), y : (Cluster, Double)) => {
			if(x._2 > y._2) x else y
		})._1
		println(clusterProbabilities.first.toList.sort((x : (Cluster, Double), y : (Cluster, Double)) => x._2 > y._2).take(20))
		val newEntropy = computeEntropy(clusterProbabilities.first)
		//println("post-expansion entropy " + newEntropy)
		if(newEntropy < entropyThreshold){
			nodesToDecide = null
			aboutToDecide = null
			chosenCluster = maxCluster
			return
		}
	
		/*
		 *Regardless of whether or not we have re-expanded, we should ask again - unless we appear done with the cluster.
		 *We calculate the information of each ask and possibly cache the new probability distributions.
		 *
		 *Need a fast algorithm to calculate the information of various asks.  We need both representative selection
		 *and the information of multiple-choice grid queries.
		 *
		 *We may have a relatively large number of nodes in clusters we are considering, especially if we are not
		 *doing much in the way of pruning.
		 *
		 *Looking for per-cluster representatives may help alleviate the problem of every node comparing with every
		 *cluster, but at the cost of potentially choosing high-overlap nodes.
		 */
		val nodes : Set[Node] = clusterProbabilities.first.keys.flatMap(_.cluster).toSet
		
		/*
		 * We want information with respect to current distribution.
		 * 
		 * This means yet another Bayesian application with prior probability.  We are asking what the probability
		 * of the cluster given the node is, and then taking the entropy of each new distribution.
		 * 
		 * We can get this by taking the prior probability of the cluster, the probability of the node, and the
		 * probability that the node would be a representative given the cluster.
		 * 
		 * What is the entropy of a node choice (as opposed to the entropy of the truth of a certain node?)
		 * It probably is a question of minimizing overlap, which might be approximable as node distances.
		 * 
		 * Since we're only going out to about 3, this is actually only n^3.  However, we would probably prefer
		 * to approximate it anyway.
		 * 
		 * We could do better by ranking mutual informations of nodes.  This is also n^3 but in relatively cheap
		 * operations (arithmetic and comparison operators).  We could also run this only for the top portion
		 * of the list in order to get a rough, probabilistic approximation.  Since mutual information is
		 * kind of complicated, we could just compute joint entropies or some other measure that respects
		 * the original entropies of the variables.
		 * 
		 * Also consider using priority queues (heaps) instead of sorting all these lists.
		 * 
		 * What is the measure we are using for a node "representing" a cluster?  It's probably the exact
		 * same measure we used earlier, except that we no longer have the notion of grid probability -
		 * we merely ask whether or not the node was chosen.  So we eliminate all measures of distance
		 * from the "chosen" node and otherwise use the previous algorithm.
		 * 
		 * Again, we ignore the word's self-probability, which is implicitly included in the normalization
		 * over all clusters.
		 * 
		 */
		val representatives = nodes.map((n : Node) =>{
			(n, normalized(clusterProbabilities.first.filter(_._1.cluster.contains(n)).map((entry : (Cluster, Double)) => {
				(entry._1, entry._2 * entry._1.averageLinkage(n))
			})))
		})
		val entropies = representatives.map((entry : (Node, Map[Cluster, Double])) => {
			(entry._1, newEntropy - entry._2.foldLeft(0.0)((acc : Double, e : (Cluster, Double)) => {
				acc + probEntropy(e._2)
			}))
		}).toList.sort((e1 : (Node, Double), e2 : (Node, Double)) => {
			e1._2 > e2._2
		}).take(entropyMax).toArray
		/*
		 * TODO: extend to consider clusters of fewer than 3 nodes, and allow arbitrary maximum numbers.
		 * 
		 * This method is supposed to find a good cover of the space.  It's trying to maximize the information
		 * gained via an ask, which means having the least overlap between choices.
		 * 
		 * A wrong way to do this is to subtract the entropy of the probability link, because this will be 0 if
		 * the link is very strong (when we want negativity).
		 * 
		 * We could make each into a binary: link total and link to other nodes in the set.  We then take the
		 * entropy of this distribution.  Unfortunately, same problem.
		 * 
		 * We could again take a distribution: how linked is the node to things that other nodes are not
		 * linked to?  This could actually be a full entropy approximation.  Unfortunately, it's not that
		 * much different from just taking the entropy at each stage, and we should find out if this is
		 * approximable.
		 * 
		 * Let the probability between 2 nodes define a mutuality between them.  Assume that this is the amount of
		 * overlap in the entropy of those 2 nodes, so subtract their average entropy times this number.
		 * 
		 * We want the answer with the MINIMUM entropy, since we are trying to destroy entropy, right?  If 1 choice
		 * were exeedingly clear, then we'd have minimal entropy.
		 * 
		 * So we want the smallest combined entropy... hmm.  We could simply apply the final iteration of our
		 * computation algorithm (simulate) in order to test where things will go.  That will make the n^3 part
		 * of this algorithm much more intensive but probably also cost less.
		 * 
		 * 
		 */		
		
		val mutualEntropies = Range(0, entropies.size).flatMap((i : Int) => {
			val e1 = entropies(i)
			Range(0, i).flatMap((j : Int) => {
				val e2 = entropies(j)
				Range(0, j).map((k : Int) => {
					val e3 = entropies(k)
					(e1._1, e2._1, e3._1, 
							e1._2 + e2._2 + e3._2 -
							((e1._2 + e2._2)/2.0) * e1._1.getProbability(e2._1) -
							((e1._2 + e3._2)/2.0) * e1._1.getProbability(e3._1) -
							((e3._2 + e2._2)/2.0) * e3._1.getProbability(e2._1)
							)
				})
			})
		}).toList.sort((e1 : (Node, Node, Node, Double), e2 : (Node, Node, Node, Double)) => {
			e1._4 > e2._4
		})

		
		//println("biggest: " + clusterProbabilities.first.keys.map(_.cluster.size).max)
		//println(entropies.toList.take(10))
		/*println(clusterProbabilities.first.toList.
						sort((e1 : (Cluster, Double), e2 : (Cluster, Double)) => {e1._2 > e2._2}).take(10).
						map(_.toString).foldLeft("\n")(_ + "\n" + _))*/
		//println(mutualEntropies.take(10))
		
		//println("Clusters updated " + clusterProbabilities.first.size)
				
		nodesToDecide = Set(mutualEntropies.first._1,
				mutualEntropies.first._2, mutualEntropies.first._3)
		
		aboutToDecide = nodesToDecide.map((n : Node) => n.getWord).toSet
		//System.out.println("About to decide between " + aboutToDecide + " " + mutualEntropies.first._1 + "_" mutualEntropies.first._2 + "_" + mutualEntropies.first._3)
}
	
	
	/**
	 * Called to undo level grid choices.
	 * 
	 */
	def rollBack = {
			chosenCluster = null
			aboutToDecide = nodesToDecide.map((n : Node) => n.getWord)
			history = history.tail
	}
}