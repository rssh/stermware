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

}
