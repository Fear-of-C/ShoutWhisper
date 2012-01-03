package fearofsoftware.bagofconcepts.lib

import net.sf.extjwnl._
import net.sf.extjwnl.data._
import net.sf.extjwnl.data.relationship._
import scala.collection.mutable.HashMap
//import java.util._
import scala.io._

/**
 * Implements:
 * 1) A clustering algorithm.  This is used for concept construction in agents and builds incrementally
 * with new input data at each step.
 * -given a word, return a list of words representing possible next clusters
 * -given a choice from that list, return a word
 * Since it is fundamentally stateful, we might encapsulate in a GridState.
 * 
 * 
 * Creating the network:
 * 
 * We're now working with Wikipedia's page links instead of WordNet.  This would ideally be based on a directed graph,
 * but I need to make this work with the clustering algorithm.  Let's first thing about the DIRECTED version though:
 * 
 * If A has a link to B, then we draw a vertex V going from A to B.  Let L be the link that sends the reader from A to B.
 * We then iterate through A and count up how many strings there are that match the text of L; let this number be
 * links(A, B.)  This is the raw value of V.
 * 
 * To make this undirected, we could just add up links(A, B) and links(B, A) to get the strength of the link between
 * A and B.  I don't see any immediate problems with this.  I'm just wondering about if there's a link from A to B
 * but no link from B to A.  That seems to suggest that if we look up A, B is a relevant fact to understanding it,
 * but that may not be the case the other way around.  I'm not sure, I've got a slightly more elegant version,
 * which is that we make the strength log(links(A, B)) + log(links(B, A)).
 * 
 * Technical note: links(A, B) > 1, since otherwise we'd get numbers that make no sense.  
 * 
 * Despite initial doubts, this equation seems to work.  It rewards equality between A and B but not too much.  This
 * can always be tweaked anyway.
 * 
 * Okay, should be relatively easy.  All good.
**/

object BagOfConcepts{
	
		def main(args: Array[String]) {
        	
			JenaModel.openModel
        	println("1. View Model \n 2. Search Model \n 3. Get Link Strength \n 4. Edit Model \n 5. Quit")
        	try{
        		Console.readInt match{
        			case 1 => JenaModel.viewModel
        				main(null)
        			case 2 => println("Enter a URI")
        				println(JenaModel.getPropertiesByURI("http://dbpedia.org/resource/" + Console.readLine))
        				main(null)
        			case 3 => println("Enter first URI")
        				val s = "http://dbpedia.org/resource/" + Console.readLine
        				println("Enter second URI")
        				val t = "http://dbpedia.org/resource/" + Console.readLine
        				println(JenaModel.getLinkByURI(s, t))
        				main(null)
        			case 4 => println("1. Add Local Data \n 2. Add External Data \n 3. Clear model")
        				try{
        					Console.readInt match{
        					case 1 => println("Enter filename")
        						JenaModel.addToModel("file:resources/rdf/" + Console.readLine)
        						main(null)
        					case 2 => println("Enter filename")
        						JenaModel.addToModel("file:/host/Users/alboland/Documents/FearOfSoftware/ShoutWhisper/dumps/" + Console.readLine)
        					case 3 => JenaModel.clearModel
        						main(null)
        					case _ => main(null)
        						}
        						
        					}
        					catch { case e: NumberFormatException => main(null) }
        				
        			case 5 => JenaModel.closeModel
        			
        			case _ => main(null)
        			}
        			
        		}
				catch { case e: NumberFormatException => main(null) }
				
			
		}
}