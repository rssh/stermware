package ua.gradsoft.termware;


/**
 * Term, which can be represent in scala/java only as reference.
 **/
trait RefTranslatedTermSignature 
{
  this: TermSignature =>

  @inline
  final def toAny(t:Term) = toAnyRef(t)

  final def fromAny(x:Any) =
    x match {
      case r: AnyRef => fromAnyRef(r)
      case _ => None
    }
  
  override def to[T](t:Term)(implicit mt:Manifest[T]): Option[T] = 
  {
    val ref = t.getRef;
    if (mt.erasure.isAssignableFrom(ref.getClass())) {
    	Some(ref.asInstanceOf[T])
    } else {
    	None
    }
  }
  
  override def from[T](x:T)(implicit mt:Manifest[T]): Option[Term] = 
  {
  	if (mt <:< Manifest.AnyVal) {
  		None
  	}else{
  		fromAnyRef(x.asInstanceOf[AnyRef])
  	}
  }
  
}
