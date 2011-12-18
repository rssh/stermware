package ua.gradsoft.termware;


/**
 * Signature for long
 */
class LongTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Long => LongTerm(x,this)
    case _ => throwUOE
  }

  val typeName = "Long" ;

  /**
   * native Long object reference.
   **/
  def toAnyRef(t:Term) = java.lang.Long.valueOf(t.getLong);

  /**
   * return long value.
   **/
  def toAny(t:Term) = t.long_! ;

  def fromAnyRef(x:AnyRef) =
   x match {
      case n: java.lang.Number => Some(LongTerm(n.longValue,this))
      case _ => None
   }

  def fromAny(x:Any) =
   x match {
      case n: Short => Some(LongTerm(n,this))
      case n: Int => Some(LongTerm(n,this))
      case n: Long => Some(LongTerm(n,this))
      case r: AnyRef => fromAnyRef(r)
      case _ => None
   }

  override def to[T](t:Term)(implicit mt:Manifest[T]): Option[T] = 
  {
  	if (mt <:< manifest[Long]) {
  		Some(t.getLong.asInstanceOf[T])
  	}else if (mt <:< manifest[BigInt]) {
  		Some(BigInt(t.getLong).asInstanceOf[T])
  	}else{
  		None
  	}
  }
  
  override def from[T](x:T)(implicit mt:Manifest[T]): Option[Term] = 
  {
  	if (mt <:< manifest[Long]) {
  		Some(new LongTerm(x.asInstanceOf[Long],this))
  	} else if (mt <:< manifest[Int]) {
  		Some(new LongTerm(x.asInstanceOf[Int].toLong,this))
  	}else{
  		None
  	}
  }
  

}
