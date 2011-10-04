package ua.gradsoft.termware;


/**
 * Signature for long
 */
class DoubleTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Double => DoubleTerm(x,this)
    case _ => throwUOE
  }

  override def typeName = "Double" ;

  /**
   * native Double object reference.
   **/
  def toAnyRef(t:Term) = java.lang.Double.valueOf(t.getDouble);

  /**
   * return double value.
   **/
  def toAny(t:Term) = t.double_! ;



}
