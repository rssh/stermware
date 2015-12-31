package termware


class ProcessingException(s:String,e:Throwable) extends RuntimeException(s,e)
{

  def this(s:String) = this(s, null)

}
