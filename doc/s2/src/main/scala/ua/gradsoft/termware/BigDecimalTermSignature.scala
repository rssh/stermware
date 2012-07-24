package ua.gradsoft.termware;


/**
 * Signature for big decimal
 */
class BigDecimalTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:BigDecimal => BigDecimalTerm(x,this)
    case _ => throwUOE
  }

  override def typeName = "BigDecimal" ;

  /**
   * native BigDecimal object.
   **/
  def toAnyRef(t:Term) = t.getBigDecimal;

  /**
   * return native BigDecimal object.
   **/
  def toAny(t:Term) = t.getBigDecimal;


  def fromAnyRef(x:AnyRef) =
   x match {
      case bd: BigDecimal => Some(BigDecimalTerm(bd,this))
      case  t: Term => if (t.isBigDecimal) Some(t) else None
      case _ => None
   }

  def fromAny(x:Any) =
   x match {
      case x: Long => fromAnyRef(BigDecimal.valueOf(x))
      case r: AnyRef => fromAnyRef(r)
      case _ => None
   }

  override def to[T](t:Term)(implicit mt:Manifest[T]): Option[T] = 
  {
  	if (mt <:< manifest[BigDecimal]) {
  		Some(t.getBigDecimal.asInstanceOf[T])
  	}else{
  		None
  	}
  }
  
  override def from[T](x:T)(implicit mt:Manifest[T]): Option[Term] = 
  {
  	if (mt <:< manifest[BigDecimal]) {
  		Some(new BigDecimalTerm(x.asInstanceOf[BigDecimal],this))
  	}else{
  		None
  	}
  }
  
  

}
