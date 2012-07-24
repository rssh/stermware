package ua.gradsoft.termware;


/**
 * Signature for long
 */
class DoubleTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Double => DoubleTerm(x,this)
    case _ => throwUOE
  }

  override def typeName = "Double" ;

  /**
   * native Double object reference.
   **/
  def toAnyRef(t:Term) = java.lang.Double.valueOf(t.getDouble);

  /**
   * return double value.
   **/
  def toAny(t:Term) = t.double_! ;

  def fromAnyRef(x:AnyRef) =
   x match {
      case n: java.lang.Number => Some(DoubleTerm(n.doubleValue,this))
      case _ => None
   }

  def fromAny(x:Any) =
   x match {
      case n: Float => Some(DoubleTerm(n,this))
      case n: Double => Some(DoubleTerm(n,this))
      case r: AnyRef => fromAny(r)
      case _ => None
   }

  override def to[T](t:Term)(implicit mt:Manifest[T]): Option[T] = 
  {
    if ( mt <:< manifest[Double] ) {
    	Some(t.getDouble.asInstanceOf[T])
    } else if (mt <:< manifest[Float]) {
    	Some(t.getDouble.toFloat.asInstanceOf[T])    	
    } else if (mt <:< manifest[java.lang.Double]) {
    	Some(java.lang.Double.valueOf(t.getDouble).asInstanceOf[T])
    } else {
    	None
    }
  }
  
  override def from[T](x:T)(implicit mt:Manifest[T]): Option[Term] = 
  {
  	if (mt <:< manifest[Double]) {
  		Some(new DoubleTerm(x.asInstanceOf[Double],this))
  	}else{
  		fromAnyRef(x.asInstanceOf[AnyRef])
  	}
  }

  



}
