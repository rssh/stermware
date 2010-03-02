package ua.gradsoft.termware;


/**
 * Signature for short
 */
class ShortTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:Short => Some(ShortTerm(x,this))
    case _ => None
  }

  def typeName = "Short" ;

}
