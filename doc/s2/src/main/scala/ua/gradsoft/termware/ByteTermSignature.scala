package ua.gradsoft.termware;


/**
 * Signature for short
 */
class ByteTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Byte => ByteTerm(x,this)
    case _ => throwUOE;
  }

  override def typeName = "Byte" ;

  /**
   * native Byte object reference.
   **/
  def toAnyRef(t:Term) = java.lang.Byte.valueOf(t.getByte);

  /**
   * return byte value.
   **/
  def toAny(t:Term) = t.byte_! ;

  def fromAnyRef(x:AnyRef) =
   x match {
      case  b: java.lang.Number => Some(new ByteTerm(b.byteValue,this))
      case  t: Term => if (t.isByte) Some(t) else None
      case _ => None
   }

  def fromAny(x:Any) =
   x match {
      case x: Byte => Some(new ByteTerm(x,this))
      case r: AnyRef => fromAnyRef(r)
      case _ => None
   }

  override def to[T](t:Term)(implicit mt:Manifest[T]):Option[T] =
  {
  	if (mt <:< Manifest.Byte) {
  		Some(t.getByte).asInstanceOf[Option[T]]
  	} else if (mt <:< manifest[java.lang.Byte]) {
  		Some(java.lang.Byte.valueOf(t.getByte)).asInstanceOf[Option[T]]
  	} else {
  		None
  	}
  }
  
  override def from[T](x:T)(implicit mt:Manifest[T]): Option[Term] =
  {
  	if (mt <:< Manifest.Byte) {
  		Some(ByteTerm(x.asInstanceOf[Byte],this));
  	} else if (mt <:< manifest[java.lang.Byte]) {	
  		Some(ByteTerm(x.asInstanceOf[java.lang.Byte].byteValue(),this));
  	} else {
  		None
  	}
  }
  
  



}


