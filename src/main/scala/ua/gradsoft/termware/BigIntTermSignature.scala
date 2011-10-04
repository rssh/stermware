package ua.gradsoft.termware;


/**
 * Signature for big integer
 */
class BigIntTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:BigInt => BigIntTerm(x,this)
    case _ => throwUOE;
  }

  override def typeName = "BigInteger" ;

  /**
   * native BigInteger object.
   **/
  def toAnyRef(t:Term) = t.getBigInt;

  /**
   * return native BigInteger object.
   **/
  def toAny(t:Term) = t.getBigInt;

  def fromAnyRef(x:AnyRef) =
   x match {
      case bi: BigInt => Some(BigIntTerm(bi,this))
      case  t: Term => if (t.isBigInt) Some(t) else None
      case _ => None
   }

  def fromAny(x:Any) =
   x match {
      case r: AnyRef => fromAnyRef(r)
      case _ => None
   }


}
