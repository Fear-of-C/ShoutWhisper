package fearofsoftware.mainsite.model

import net.liftweb.mapper._

object PollResult extends PollResult with LongKeyedMetaMapper[PollResult]{
	override def dbTableName = "poll_results"
}

class PollResult extends LongKeyedMapper[PollResult]{
	def getSingleton = PollResult
	
	def primaryKeyField = id
	
	object id extends MappedLongIndex(this)
	object resultString extends MappedString(this, 100)
	//find out how long this should actually be
	//we will probably map short keys (not names) and keep a separate table
	//which means we are giving this pseudo-# 100 digits (so up to a googol possible configurations)
	object pollId extends MappedLong(this){
		override def dbIndexed_? = true
	}
	//may want to hold multiple polls - this accounts for that
	//definitely want to be able to search by poll type
	//some separate table will keep track of which # corresponds to which name
	
	object user extends LongMappedMapper(this, User)
}