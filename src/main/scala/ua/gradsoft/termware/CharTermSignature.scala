package ua.gradsoft.termware;

/**
 * Signature for character
 */
class CharTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Char => new CharTerm(x,this)
    case _ => throwUOE;
  }

  override def typeName = "Char" ;

 /**
   * native Char object reference.
   **/
  def toAnyRef(t:Term) = java.lang.Character.valueOf(t.getChar);

  /**
   * return char value.
   **/
  def toAny(t:Term) = t.char_! ;

  def fromAnyRef(x:AnyRef) =
   x match {
      case  c: java.lang.Character => Some(CharTerm(c,this))
      case  t: Term => if (t.isChar) Some(t) else None
      case _ => None
   }

  def fromAny(x:Any) =
   x match {
      case c: Char => Some(CharTerm(c,this))
      case r: AnyRef => fromAnyRef(r)
      case _ => None
   }

  override def to[T](t:Term)(implicit mt:Manifest[T]):Option[T] =
  {
  	if (mt <:< Manifest.Char) {
  		Some(t.getChar).asInstanceOf[Option[T]]
  	} else if (mt <:< manifest[java.lang.Character]) {
  		Some(java.lang.Character.valueOf(t.getChar)).asInstanceOf[Option[T]]
  	} else {
  		None
  	}
  }
  
  override def from[T](x:T)(implicit mt:Manifest[T]): Option[Term] =
  {
  	if (mt <:< Manifest.Char) {
  		Some(CharTerm(x.asInstanceOf[Char],this));
  	} else if (mt <:< manifest[java.lang.Character]) {	
  		Some(CharTerm(x.asInstanceOf[java.lang.Character].charValue(),this));
  	} else {
  		None
  	}
  }
  


}
