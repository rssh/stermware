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


}


