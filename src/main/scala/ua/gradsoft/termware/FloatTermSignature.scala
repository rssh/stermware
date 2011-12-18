package ua.gradsoft.termware;

/**
 * Signature for float
 */
class FloatTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Float => FloatTerm(x,this)
    case _ => throwUOE;
  }

  override def typeName = "Float" ;

 /**
   * native Float object reference.
   **/
  def toAnyRef(t:Term) = java.lang.Float.valueOf(t.getFloat);

  /**
   * return float value.
   **/
  def toAny(t:Term) = t.float_! ;

 def fromAnyRef(x:AnyRef) =
   x match {
      case n: java.lang.Number => Some(FloatTerm(n.floatValue,this))
      case t: Term => if (t.isFloat) Some(t) else None
      case _ => None
   }

  def fromAny(x:Any) =
   x match {
      case n: Float => Some(FloatTerm(n,this))
      case r: AnyRef => fromAny(r)
      case _ => None
   }

  override def to[T](t:Term)(implicit mt:Manifest[T]): Option[T] = 
  {
    if ( mt <:< manifest[Float] ) {
    	Some(t.getFloat.asInstanceOf[T])
    } else if (mt <:< manifest[Double]) {
    	Some(t.getFloat.toDouble.asInstanceOf[T])
    } else if (mt <:< manifest[java.lang.Float]) {
    	Some(java.lang.Float.valueOf(t.getFloat).asInstanceOf[T])
    } else {
    	None
    }
  }
  
  override def from[T](x:T)(implicit mt:Manifest[T]): Option[Term] = 
  {
  	if (mt <:< manifest[Float]) {
  		Some(new FloatTerm(x.asInstanceOf[Float],this))
  	}else{
  		fromAnyRef(x.asInstanceOf[AnyRef])
  	}
  }




}
