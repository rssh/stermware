package ua.gradsoft.termware;


/**
 * Signature for big integer
 */
class BigIntTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:BigInt => Some(BigIntTerm(x,this))
    case _ => None
  }

  override def typeName = "BigInteger" ;

}
