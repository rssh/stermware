package ua.gradsoft.termware;


/**
 * Signature for short
 */
class ShortTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Short => ShortTerm(x,this)
    case _ => throwUOE;
  }

  def typeName = "Short" ;

}
