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

  def to[T](t:Term)(implicit mt:Manifest[T]):Option[T] =
  {
  	if (mt <:< Manifest.Int) {
  		Some(t.getInt).asInstanceOf[Option[T]]
  	} else if (mt <:< Manifest.Long) {
  		Some(t.getLong).asInstanceOf[Option[T]]
  	} else if (mt <:< manifest[java.lang.Integer]) {
  		Some(new java.lang.Integer(t.getInt)).asInstanceOf[Option[T]]
  	} else if (mt <:< manifest[BigInt]) {
  		Some(BigInt(t.getLong)).asInstanceOf[Option[T]]
  	} else {
  		None
  	}
  }
  
  override def from[T](x:T)(implicit mt:Manifest[T]): Option[Term] =
  {
  	if (mt <:< Manifest.Int) {
  		Some(new IntTerm(x.asInstanceOf[Int],this));
  	} else if (mt <:< Manifest.Short) {
  		Some(new IntTerm(x.asInstanceOf[Short].toInt,this));
  	} else if (mt <:< manifest[java.lang.Number]) {	
  		Some(new IntTerm(x.asInstanceOf[java.lang.Number].intValue(),this));
  	} else {
  		None
  	}
  }
  
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
