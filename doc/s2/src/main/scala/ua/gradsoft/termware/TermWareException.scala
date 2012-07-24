package ua.gradsoft.termware;

/**
 * base of all termware exceptions.
 **/
class TermWareException(message:String, c:Int, ex:Throwable) 
                                       extends RuntimeException(message,ex)
{

  def this(message:String) = this(message, -1, null);

  def this(message:String, ex:Throwable) = this(message, -1, ex);
  
  /**
   * get exception code.
   **/
  def getCode: Option[Int] = 
                   if (code == -1) None else Some(code);
  
  private val code = c;
}
