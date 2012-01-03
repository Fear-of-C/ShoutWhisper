package fearofsoftware.bagofconcepts.lib.neo

import org.neo4j.kernel.impl.batchinsert.BatchInserter
import org.neo4j.kernel.impl.batchinsert.BatchInserterImpl
import org.neo4j.index.impl.lucene.LuceneBatchInserterIndexProvider
import org.neo4j.helpers.collection.MapUtil
import org.neo4j.graphdb.RelationshipType
import org.neo4j.graphdb.Node
import scala.collection.JavaConversions._
import java.util.regex.Pattern
import scala.util.matching.Regex
import org.neo4j.graphdb.index.BatchInserterIndex
import org.neo4j.kernel.EmbeddedGraphDatabase

/**
 * Questions:
 * 1) What are categories?  Are they also terms?
 */
object NeoModel {
	
	private def getScanner(s : String, buffSize : Int) = {
				new java.util.Scanner(new java.io.BufferedReader(new java.io.FileReader(new java.io.File(s)), buffSize))
	}
	
	private def identFromURL(url : String) = {
		val regex = new Regex("<http://dbpedia.org/resource/(.*?)>")
		//TODO: something more interesting with portals
		val regex(identifier) = url
		identifier.toLowerCase
	}
	
	private val PreScanRegex = new Regex("^(<.*?>)\\s*<.*?>\\s*\"(.*?)\"@(.*?)\\s*\\.$")
	
	def loadWords(inserter : BatchInserterImpl, wordIndex : BatchInserterIndex, identIndex : BatchInserterIndex, scanner : java.util.Scanner) = {
		var totalWordCount = 0L
		while(scanner.hasNextLine){
			val PreScanRegex(url, text, language) = scanner.nextLine
			val ident = identFromURL(url)
			val node = inserter.createNode(MapUtil.map("text", text,
					"language", language, "link",
					ident))
			wordIndex.add(node, MapUtil.map("text", text))
			identIndex.add(node, MapUtil.map("language", language, "link", ident))
			totalWordCount += 1
			if((totalWordCount % 100000L) == 0){
				println("Indexed " + totalWordCount + " words.")
			}
		}
		println("Finished loading " + totalWordCount + " words.")
		totalWordCount
	}

	def loadArticleTitles(file : String, inserter : BatchInserterImpl, wordIndex : BatchInserterIndex, identIndex : BatchInserterIndex) = {
		//100MB buffer
		val preScanner = getScanner(file, 100000000)
		val ret = loadWords(inserter, wordIndex, identIndex, preScanner)
		preScanner.close
		ret
	}
	
	def loadCategories(file : String, inserter : BatchInserterImpl, wordIndex : BatchInserterIndex, identIndex : BatchInserterIndex) = {
		//100MB buffer
		val scanner = getScanner(file, 100000000)
		//val regex = new Regex("^(<.*?>)\\s*<.*?>\\s*(<.*?>)\\s*\\.$")
		val ret = loadWords(inserter, wordIndex, identIndex, scanner)
		ret
	}
	
	private def getSingleNode(index : BatchInserterIndex, key :String, value : Any) : java.lang.Long = {
		val nodes = index.get(key, value)
		val node = if(nodes.size > 1){
			val newSet = new scala.collection.mutable.HashSet[java.lang.Long]()
			while(nodes.hasNext){
				newSet += nodes.next
			}
			println("Got " + newSet.size + " nodes for " + value)
			newSet.toList.sort((x : java.lang.Long, y : java.lang.Long) => x.longValue < y.longValue).first
		}else{
			nodes.getSingle
		}
		nodes.close
		node
	}
	
	/**
	 * Precondition: all nodes on which links are to be defined are already indexed.
	 */
	def loadRel(inserter : BatchInserterImpl, wordIndex : BatchInserterIndex, scanner : java.util.Scanner,
			t : RelTypes) : Long = {
		
		def fromScanLine= {
			val regex = new Regex("^(<.*?>)\\s*<.*?>\\s*(<.*?>)\\s*\\.$")
			val regex(from, to) = scanner.nextLine()
			(identFromURL(from), identFromURL(to))
		}
		
		var fromNode : (String, Long) = (null, -1)
		var totalLinkCount = 0L
		var totalRejectedCount = 0L
		while(scanner.hasNextLine){
			var (from, to) = fromScanLine
			if(fromNode._1 != from){
				var nodeFrom = getSingleNode(wordIndex, "link", from)
				while(nodeFrom == null){
					val oldFrom = from
					while(from == oldFrom){
						if(!scanner.hasNextLine){
							println("Finished adding " + totalLinkCount + " relationships.")
							println("Rejected " + totalRejectedCount + " relationships.")
							return totalLinkCount
						}
						val (nFrom, nTo) = fromScanLine
						from = nFrom
						to = nTo
						totalRejectedCount += 1
					}
					nodeFrom = getSingleNode(wordIndex, "link", from)
				}
				fromNode = (from, nodeFrom.longValue)
			}
			val nodeTo = getSingleNode(wordIndex, "link", to)
			if(nodeTo != null){
				inserter.createRelationship(fromNode._2, nodeTo.longValue,
						t, MapUtil.map())
				totalLinkCount += 1
				if((totalLinkCount % 100000) == 0){
					println("Added " + totalLinkCount + " relationships.")
				}
			}else{
				totalRejectedCount += 1
			}
		}
		println("Finished adding " + totalLinkCount + " relationships.")
		println("Rejected " + totalRejectedCount + ".")
		totalLinkCount
	}
	
	def loadDisambiguations(file : String, inserter : BatchInserterImpl, wordIndex : BatchInserterIndex) = {
		//100MB buffer
		val scanner = getScanner(file, 100000000)
		loadRel(inserter, wordIndex, scanner, RelTypes.WIKIPEDIA_DISAMBIGUATION)
	}
	
	/*def loadRedirects(file : String, inserter : BatchInserterImpl, wordIndex : BatchInserterIndex) = {
		//100MB buffer
		val scanner = getScanner(file, 100000000)
		val regex = new Regex("^(<.*?>)\\s*<.*?>\\s*(<.*?>)\\s*\\.$")
		var totalRedirects = 0L
		while(scanner.hasNextLine){
			val regex(to, from) = scanner.nextLine
			val properties = MapUtil.map("language", "en", "link", from,
					"text", from.split("/").last)
			val fromNode = (from, inserter.createNode(properties))
			wordIndex.add(fromNode._2, properties)
			//the node pointed to should already exist in the database
			val nodeTo = getSingleNode(wordIndex, "link", to)
			if(nodeTo != null){
				inserter.createRelationship(fromNode._2, nodeTo.longValue,
						RelTypes.WIKIPEDIA_REDIRECT, MapUtil.map())
				totalRedirects += 1
				if((totalRedirects % 100000) == 0){
					println("Added " + totalRedirects + " redirects.")
				}
			}
		}
		println("Finished adding " + totalRedirects + " redirects.")
		totalRedirects
	}*/
	def loadRedirects(file : String, inserter : BatchInserterImpl, wordIndex : BatchInserterIndex) = {
		//100MB buffer
		val scanner = getScanner(file, 100000000)
		val ret = loadRel(inserter, wordIndex, scanner, RelTypes.WIKIPEDIA_REDIRECT)
		scanner.close
		ret
	}
	
	def loadRelationships(file : String, inserter : BatchInserterImpl, wordIndex : BatchInserterIndex) = {
		//100 MB buffer
		val scanner = getScanner(file, 100000000)
		val ret = loadRel(inserter, wordIndex, scanner, RelTypes.WIKIPEDIA_LINK)
		scanner.close
		ret
	}
	
	
	private val fileDirectory = "/home/nick/Public/"
	
	def load = {
		val dbDirectory = fileDirectory + "bagofconceptsdb"
		val inserter = new BatchInserterImpl(dbDirectory);
		val indexProvider = new LuceneBatchInserterIndexProvider(inserter)
		val wordIndex = indexProvider.nodeIndex("terms", MapUtil.stringMap("type", "fulltext", "to_lower_case", "true"))
		val identIndex = indexProvider.nodeIndex("ident", MapUtil.stringMap("type", "exact", "to_lower_case", "true"))
		val properties = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(fileDirectory + "properties")))
		//loadDBPedia(fileDirectory + "bagofconceptsdb", fileDirectory + "page_links_en.nt",
		//		fileDirectory + "labels_en.nt", "bagofconceptsprops")
		println("Loading titles...")
		properties.write("titles = " + loadArticleTitles(fileDirectory + "labels_unique_en.nt", inserter, wordIndex, identIndex) + "\n")
		println("Loading categories...")
		properties.write("categories = " + loadCategories(fileDirectory + "category_labels_unique_en.nt", inserter, wordIndex, identIndex) + "\n")
		identIndex.setCacheCapacity("link", 100000)
		wordIndex.flush
		identIndex.flush
		println("Loading disambiguations...")
		properties.write("disambiguations = " + loadDisambiguations(fileDirectory + "disambiguations_en.nt", inserter, identIndex) + "\n")
		println("Loading links...")
		properties.write("links = " + loadRelationships(fileDirectory + "page_links_en.nt", inserter, identIndex) + "\n")
		println("Loading redirects...")
		properties.write("redirects = " + loadRedirects(fileDirectory + "redirects_unique_en.nt", inserter, identIndex) + "\n")
		println("Done loading... shutting down.")
		indexProvider.shutdown
		inserter.shutdown
		properties.close
		println("Finished.")
	}
	
	def main(args : Array[String]) = {
		//load the database
		val propertiesScanner = getScanner(fileDirectory + "propertiesfinished_1", 1000000)
		val properties = scala.collection.mutable.Map[String, Long]()
		val regex = new Regex("^(.+?)\\s*=\\s*(\\d+)$")
		while(propertiesScanner.hasNextLine){
			val regex(name, value) = propertiesScanner.nextLine
			properties.put(name, value.toLong)
		}
		val db = new EmbeddedGraphDatabase(fileDirectory + "dbfinished_1")
		val factory = new NeoNodeFactory(db, properties("links"), properties("titles"))
		//now we can attempt to run some tests
		//let's calculate some numerical properties of the graph
		//can we iterate nodes?
		val index = db.index.forNodes("ident")
		//index.get()
		val linkCounts = new JMapWrapper(new java.util.HashMap[Long, Long](1000)) {
			override def default(key : Long) = 0L
		}
		/*var count = 0L
		val fileScanner = getScanner(fileDirectory + "labels_unique_en.nt", 100000000)
		while(fileScanner.hasNext){
			val PreScanRegex(url, text, language) = fileScanner.nextLine
			val ih = index.get("link", identFromURL(url))
			val node = ih.getSingle
			ih.close
			linkCounts(node.getRelationships(RelTypes.WIKIPEDIA_LINK).size) += 1
			if((count % 1000) == 0){
				println("Counted links on " + count + " nodes with table size " + linkCounts.size)
			}
			count += 1
		}
		fileScanner.close*/
		var count = 0L
		//var tx = db.beginTx
		db.getAllNodes.foreach(node => {
			linkCounts(node.getRelationships.size) += 1
			if((count % 1000) == 0){
				println("Counted links on " + count + " nodes with table size " + linkCounts.size + ".")
			}
			if(((count % 50000) == 0) && (count != 0)){
				//tx.success
				//tx.finish
				//tx = db.beginTx
				val linkProps = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(fileDirectory + "linkProps")))
				linkCounts.toList.sort((x : (Long, Long), y : (Long, Long)) => x._1 < y._1).foreach{
					case (links, count) => linkProps.write(links + " - " + count + "\n")
				}
				linkProps.close
			}
			count += 1
		})
		//tx.success
		//tx.finish
		
		db.shutdown
	}
}






















