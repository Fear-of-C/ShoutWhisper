package fearofsoftware.bagofconcepts.lib.neo;

import org.neo4j.graphdb.RelationshipType;

public enum RelTypes implements RelationshipType {
	WIKIPEDIA_LINK, WIKIPEDIA_DISAMBIGUATION, WIKIPEDIA_REDIRECT
}
