package fearofsoftware.bagofconcepts.lib

import java.sql._
import com.hp.hpl.jena._
import com.hp.hpl.jena.db._
import com.hp.hpl.jena.rdf.model._
import scala.collection.mutable.HashMap
import com.hp.hpl.jena.query.Dataset
//import com.hp.hpl.jena.tdb.TDBFactory


object JenaModel{
	
	def DB_URL = "jdbc:mysql://localhost/jenamodel"
	def DB_USER = "root"
	def DB_PASSWD = "[[m*vip]]"
	def DB_TYPE = "MySQL"
	var openedModel: Model = null
	var ds : Dataset = null
	
	def predicates = Map("wikiPageWikiLink" -> "link", "wikiPageRedirects" -> "synonym",
												"wikiPageDisambiguates" -> "synonym")
	
		
	
	
	
	
	
	
	def openModel{
		
		var assemblerFile = "file:resources/tdb/tdb-assembler.ttl"
		//openedModel = TDBFactory.assembleModel(assemblerFile)
		
	}
	
	def viewModel{
		openedModel.write(System.out)
	}
	
	def addToModel(fileName: String) = {
		openedModel.read(fileName, "N-TRIPLE")
	}
	
	def getOpenedModel = {
		openedModel
	}
	
	def clearModel = {
		openedModel.removeAll()
	}
	
	
	def closeModel = { openedModel.close() }
	
	/*
	 * Mapping Algorithms
	 */
	
	
	
	def getPropertiesByURI(u: String) = {
		getProperties(openedModel.getResource(u))
	}
	
	def getProperties(u: Resource) = {
		def props: StmtIterator = u.listProperties()
		val propList = props.toList()
		
		var newList : List[com.hp.hpl.jena.rdf.model.Statement] = Nil
		
		/*
		 * I know putting a for loop here is weird, but the iterator is doing weird stuff
		 * The iterator seems outright busted, so using its toList() method to get the info needed
		 */
		for (i <- 0.until(propList.size()))
			newList = propList.get(i) :: newList
		
		newList
	}
	
		
	
	def getLinkByURI(s: String, t: String) = {
		getLink(openedModel.getResource(s), openedModel.getResource(t))
	}
	
	/*
	 * This method indiscriminately counts the links going between resources u and v
	 * i.e. it doesn't care about the direction of the link or any of its properties
	 */
	def getLink(u: Resource, v: Resource): Int = {
		
		var uSyns = getSynonyms(u)
		var vSyns = getSynonyms(v)
		
		var uLinks = getProperties(u).filter(i => i.getPredicate().getLocalName().equals("wikiPageWikiLink"))
		var vLinks = getProperties(v).filter(i => i.getPredicate().getLocalName().equals("wikiPageWikiLink"))
		
		println("uSyns: " + uSyns)
		
		/*
		 * Notes on what's removed:
		 * 		u and v are by default removed, since they're not in their own synonym list
		 * 		Whenever function is called on uSynsr-type chaos mode(i) (WLOG), uSyns(i) is removed from the synonym list of that function call; this is because 
		 * 		we don't want something treating itself as its own synonym
		 */
		var SynLinks = 0
		for (i <- 0.until(uSyns.size))
		{
			SynLinks += getLink(uSyns(i).getObject().asResource(), v, uSyns - uSyns(i), vSyns)
			for (j <- 0.until(vSyns.size))
				SynLinks += getLink(uSyns(i).getObject().asResource(), vSyns(j).getObject.asResource(), uSyns - uSyns(i), vSyns - vSyns(j))
		}
		for (i <- 0.until(vSyns.size))
			SynLinks += getLink(u, vSyns(i).getObject().asResource(), uSyns, vSyns - vSyns(i))
		
		/*
		 * This base case gives double strength to mutual linking.  Should we keep that?
		 */
		def uvLinks = uLinks.filter(i => i.getObject().asResource().getURI().equals(v.getURI()))
		def vuLinks = vLinks.filter(i => i.getObject().asResource().getURI().equals(u.getURI()))	
		
		uvLinks.size + vuLinks.size + SynLinks
	}
	
	
	/*
	 * Overloaded version of getLink: allows initial version to remove nodes at each recursive step
	 */
	def getLink(u: Resource, v: Resource, uSyns: List[com.hp.hpl.jena.rdf.model.Statement], vSyns: List[com.hp.hpl.jena.rdf.model.Statement]) : Int = {
		
		var uLinks = getDirectLinks(u)
		var vLinks = getDirectLinks(v)
		
		var SynLinks = 0
		for (i <- 0.until(uSyns.size))
		{
			SynLinks += getLink(uSyns(i).getObject().asResource(), v, uSyns - uSyns(i), vSyns)
			for (j <- 0.until(vSyns.size))
				SynLinks += getLink(uSyns(i).getObject().asResource(), vSyns(j).getObject.asResource(), uSyns - uSyns(i), vSyns - vSyns(j))
		}
		for (i <- 0.until(vSyns.size))
			SynLinks += getLink(u, vSyns(i).getObject().asResource(), uSyns, vSyns - vSyns(i))
		
		
		def uvLinks = uLinks.filter(i => i.getObject().asResource().getURI().equals(v.getURI()))
		def vuLinks = vLinks.filter(i => i.getObject().asResource().getURI().equals(u.getURI()))	
		
		uvLinks.size + vuLinks.size + SynLinks
	}
	
	
	/*
	 * Accessor method for getting synonyms, not currently used
	 */
	def getSynonyms(u: Resource) =	getProperties(u).filter(i => predicates(i.getPredicate.getLocalName()).equals("synonym"))
	
	def getDirectLinks(u: Resource) = getProperties(u).filter(i => predicates(i.getPredicate.getLocalName()).equals("link"))
}

