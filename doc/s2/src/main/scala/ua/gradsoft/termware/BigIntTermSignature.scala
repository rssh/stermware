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

  override def to[T](t:Term)(implicit mt:Manifest[T]): Option[T] = 
  {
  	if (mt <:< manifest[BigInt]) {
  		Some(t.getBigInt.asInstanceOf[T])
  	}else if (mt <:< manifest[Long]) {
  		Some(t.getBigInt.longValue().asInstanceOf[T])
  	}else{
  		None
  	}
  }
  
  override def from[T](x:T)(implicit mt:Manifest[T]): Option[Term] = 
  {
  	if (mt <:< manifest[BigInt]) {
  		Some(new BigIntTerm(x.asInstanceOf[BigInt],this))
  	}else{
  		None
  	}
  }
  
  

}
