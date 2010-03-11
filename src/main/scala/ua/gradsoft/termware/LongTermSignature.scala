package ua.gradsoft.termware;


/**
 * Signature for long
 */
class LongTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Long => LongTerm(x,this)
    case _ => throwUOE
  }

  val typeName = "Long" ;

}
