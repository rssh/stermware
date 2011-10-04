package ua.gradsoft.termware;


/**
 * Signature for big integer
 */
class BigIntTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:BigInt => BigIntTerm(x,this)
    case _ => throwUOE;
  }

  override def typeName = "BigInteger" ;

  /**
   * native BigInteger object.
   **/
  def toAnyRef(t:Term) = t.getBigInt;

  /**
   * return native BigInteger object.
   **/
  def toAny(t:Term) = t.getBigInt;


}
