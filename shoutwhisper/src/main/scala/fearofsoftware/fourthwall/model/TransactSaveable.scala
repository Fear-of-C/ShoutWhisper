package fearofsoftware.fourthwall.model

trait TransactSaveable {

	def save : Boolean
		
	def nestedSave = save
	
	def selfSave = save
	
	def delete_! : Boolean
	
	def nestedDelete  = delete_!
	
	def selfDelete = delete_!
}