package fearofsoftware.fourthwall.model

import net.liftweb.mapper._
import java.util.Date
import net.liftweb.common.Box
import net.liftweb.actor.LiftActor
import math.sqrt
import fearofsoftware.fourthwall.lib.input.Privacy

/**
 * Represents an article in the system.  Anything that has text can be an article.
 */
trait Article extends Discourse{

	def articleTraits = Array("text")
	override def self = this.asInstanceOf[DiscourseObject]
	
	def isArticle = {
		articleTraits.forall(self.has(_))
	}
	
	var wordBagCache : Map[String, Int] = null
	/**
	 * We don't want to make a new wordbag every time that this method is called.  Let's cache this and figure out
	 * separately how to invalidate it.
	 */
	def makeWordBag : Map[String, Int]= {
		if(wordBagCache == null){
			wordBagCache = (if(has("text")){
				val wordBag = scala.collection.mutable.Map[String, Int]()
				def wordFound(word : String) = wordBag synchronized(wordBag.put(word, if(wordBag.contains(word)) wordBag(word) + 1 else 0))
				self.get[String]("text").split("\\s+").foreach(wordFound(_))
				wordBag.toMap
			} else Map[String, Int]())
		}
		wordBagCache
	}
	
	def getOwners : Array[DiscourseObject]= {
		if(has("privacy") && get[Long]("privacy") == Privacy.priv)
			self.get[RelationTraitImpl]("owners").subjects.toArray else null
	}
	
	/**
	 * Searches by a plain word match.
	 */
	def findMatching : Set[Agent] = {
		if(!self.has("text")){
			return Set[Agent]()
		}
		//1st, we'll break down this article into a word list, using a map with synchronized puts
		
		
		//then, we make a list out of the keys from that map for use in the SQL statement
		//this means generating a string with the correct number of arguments from right here
		// http://www.javaranch.com/journal/200510/Journal200510.jsp#a2
		val wordBag = makeWordBag
		
		println("Searching for article with words " + wordBag.keys)
		
		val traitString = "('privacy', 'text')"
		val variableQueryString =
			"(" + wordBag.keys.toList.map(w => "?").mkString(",") + ")"
		val owners : Array[DiscourseObject] = getOwners
		println("owners: " + owners)
		val sqlString =  "SELECT DiscourseObject.* FROM " +
				"DiscourseObject JOIN Trait " +
				"ON DiscourseObject.id = Trait.discourse " + 
				"JOIN TraitName ON TraitName.id = Trait.Name " +
				/*
				 * Bind trait implementations to traits
				 */
				"JOIN TextTraitImpl ON TextTraitImpl.tr = Trait.id " +
				/*
				 * Get all word uses for each trait implementation
				 */
				"JOIN WordUse ON WordUse.text = TextTraitImpl.id " +
				/*
				 * Bind words to use word uses
				 */
				"JOIN Word ON Word.id = WordUse.word " +
				"WHERE TraitName.name IN ('text') AND " +
				"Word.text IN " + variableQueryString + ";"
		/*val testOwners = "(" + owners.toList.map(o => "?").mkString(",") + ")"		
		val testRes = DiscourseObject.findAllByPreparedStatement(conn => {
		  conn.prepareStatement("SELECT agent.* FROM " +
				"DiscourseObject agent JOIN Trait " +
				"ON agent.id = Trait.discourse " + 
				"JOIN TraitName ON TraitName.id = Trait.name " +
				/*
				 * This ownership/privacy block is causing the bug
				 */
				"JOIN RelationTraitImpl ON RelationTraitImpl.tr = Trait.id " +
				"JOIN Relation ON Relation.tr = RelationTraitImpl.id " +
				"JOIN DiscourseObject owner ON owner.id = Relation.subject " +
				"WHERE TraitName.name IN ('owners')" +
				//"AND Word.text IN " + variableQueryString +
				//"AND owner.id IN " + testOwners +
				";")
		})
		println("testRes: " + testRes)*/
				
		val res = DiscourseObject.findAllByPreparedStatement(conn => {
				val ps = conn.prepareStatement(sqlString)
				val words = wordBag.keys.toArray
				Range(0, words.length).foreach(i => {
				  		//I forgot to make the length of this variable depending
				  		//on how many trait names there were in the list
					ps.setString(i + 1, words(i))
				})
				ps
		}).toSet[Agent]
		
		
		
		if(owners != null){
			res.filter(result => if (result.asInstanceOf[Article].getOwners != null) {
				!res.asInstanceOf[Article].getOwners.forall(!owners.contains(_))
			} else true)
		} else res
		
	}
	
	def wordBagCompare(other : Article) : Double = {
		val myWords = makeWordBag
		val theirWords = other.makeWordBag
		if(myWords.isEmpty || theirWords.isEmpty){
			if(myWords.isEmpty && theirWords.isEmpty) {return 1.0} else {return 0.0}
		}
		if(myWords.size <= theirWords.size){
			myWords.map{case (word, count) => {
				if(theirWords.contains(word)){
					count * theirWords(word)
				} else 0.0
			}}.sum/sqrt(myWords.values.map(c => c*c).sum * theirWords.values.map(c => c*c).sum)
		} else other.wordBagCompare(this)
	}
	
	def findUniversal : Set[Agent] = {
		val owners = getOwners
		/*if(owners != null){
			println("articles's trait owners are " + owners.toList)
		}*/

		val sqlString = if(owners == null){
			"SELECT DiscourseObject.* FROM DiscourseObject " +
				"JOIN UniversalAgents ON UniversalAgents.agentId = DiscourseObject.id;"
		}else{
			"SELECT agent.* FROM DiscourseObject agent " +
				"JOIN UniversalAgents ON UniversalAgents.agentId = agent.id " +
				"JOIN Trait ON Trait.discourse = agent.id " +
				"JOIN TraitName ON Trait.name = TraitName.id " +
				"JOIN RelationTraitImpl ON RelationTraitImpl.tr = Trait.id " +
				"JOIN Relation ON Relation.tr = RelationTraitImpl.id " +
				"JOIN DiscourseObject owners ON owners.id = Relation.subject " +
				"WHERE TraitName.name = 'owners' " +
				"AND owners.id IN " + "(" + owners.map(o => "?").mkString(",") + ");"
		}
		//println("sql is: " + sqlString)
		DiscourseObject.findAllByPreparedStatement(conn => {
			val ps = conn.prepareStatement(sqlString)
			if(owners != null){
				Range(0, owners.size).foreach(i => {
					ps.setLong(i + 1, owners(i).id.is)
				})
			}
			//println(ps)
			ps
		}).toSet
	}
	
	def findRelevant : Set[Agent] = {
		var result : Set[Agent] = null
		DiscourseObject.transact(Unit => {
			result = if(({
				val owners = getOwners
				(owners != null) && owners.isEmpty})) 
				Set[Agent]() else findUniversal ++ findMatching
			true
		})
		result
	}
}












