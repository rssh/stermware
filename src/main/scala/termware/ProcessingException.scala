package termware


class ProcessingException(
           val message:String, 
           val tracking: Seq[String], 
           val cause:Throwable) extends RuntimeException(message,cause)
{

  def this(message:String) = this(message, Seq(), null)

  def this(message:String, tracking:Seq[String]) = this(message, tracking, null)
 
}
