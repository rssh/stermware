package ua.gradsoft.termware;


/**
 * Signature for long
 */
class DoubleTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:Double => Some(DoubleTerm(x,this))
    case _ => None
  }

  override def typeName = "Double" ;

}
