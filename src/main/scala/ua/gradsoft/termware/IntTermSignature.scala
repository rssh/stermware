package ua.gradsoft.termware;


/**
 * Signature for int
 */
class IntTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Int => IntTerm(x,this)
    case x:java.lang.Integer => IntTerm(x.intValue,this)
    case _ => throwUOE;
  }

  override def typeName = "Int" ;

  /**
   * native Int object reference.
   **/
  def toAnyRef(t:Term) = java.lang.Integer.valueOf(t.getInt);

  /**
   * return int value.
   **/
  def toAny(t:Term) = t.int_! ;

  def fromAnyRef(x:AnyRef) =
   x match {
      case n: java.lang.Number => Some(IntTerm(n.intValue,this))
      case _ => None
   }

  def fromAny(x:Any) = 
   x match {
      case n: Int => Some(IntTerm(n,this))
      case r: AnyRef => fromAny(r)
      case _ => None
   }

}
