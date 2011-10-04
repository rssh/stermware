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

  def fromAnyRef(x:AnyRef) =
   x match {
      case n: java.lang.Number => Some(DoubleTerm(n.doubleValue,this))
      case _ => None
   }

  def fromAny(x:Any) =
   x match {
      case n: Float => Some(DoubleTerm(n,this))
      case n: Double => Some(DoubleTerm(n,this))
      case r: AnyRef => fromAny(r)
      case _ => None
   }




}
