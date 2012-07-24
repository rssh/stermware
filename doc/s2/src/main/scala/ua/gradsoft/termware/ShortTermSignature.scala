package ua.gradsoft.termware;


/**
 * Signature for short
 */
class ShortTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def to[T](t:Term)(implicit mt:Manifest[T]) =
  {
  	if (mt <:< Manifest.Short) {
  		Some(t.getShort.asInstanceOf[T])
  	}else{
  		None
  	}
  }
  
  override def from[T](x:T)(implicit mt:Manifest[T]):Option[Term] =
  {
  	if (mt <:< Manifest.Short) {
  		Some(new ShortTerm(x.asInstanceOf[Short],this));
  	} else if (mt <:< manifest[Number]) {
  	    Some(new ShortTerm(x.asInstanceOf[Number].shortValue(),this));
  	} else {
  		None
  	}
  }
	
  override def createConstant(arg:Any):Term = arg match {
    case x:Short => ShortTerm(x,this)
    case _ => throwUOE;
  }

  def typeName = "Short" ;

  /**
   * native Short object reference.
   **/
  def toAnyRef(t:Term) = java.lang.Short.valueOf(t.getShort);

  /**
   * return short value.
   **/
  def toAny(t:Term) = t.short_! ;

  def fromAnyRef(x:AnyRef) =
   x match {
      case n: java.lang.Number => Some(ShortTerm(n.shortValue,this))
      case _ => None
   }

  def fromAny(x:Any) =
   x match {
      case n: Short => Some(ShortTerm(n,this))
      case r: AnyRef => fromAny(r)
      case _ => None
   }


}
