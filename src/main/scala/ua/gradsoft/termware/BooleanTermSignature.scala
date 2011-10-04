package ua.gradsoft.termware;

/**
 * Signature for boolean
 */
class BooleanTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Boolean => BooleanTerm(x,this)
    case _ => throwUOE;
  }

  override def typeName = "Boolean";

  /**
   * native Boolean object reference.
   **/
  def toAnyRef(t:Term) = java.lang.Boolean.valueOf(t.getBoolean);

  /**
   * return native boolean.
   **/
  def toAny(t:Term) = t.boolean_! ;

  def fromAnyRef(x:AnyRef) =
   x match {
      case  b: java.lang.Boolean => Some(BooleanTerm(b,this))
      case  t: Term => if (t.isBoolean) Some(t) else None
      case _ => None
   }

  def fromAny(x:Any) =
   x match {
      case b: Boolean => Some(new BooleanTerm(b,this))
      case r: AnyRef => fromAnyRef(r)
      case _ => None
   }


}
