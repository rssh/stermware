package ua.gradsoft.termware;


/**
 * Signature for big integer
 */
class BigIntTermSignature(th:Theory) extends PrimitiveTermSignature
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:BigInt => Some(BigIntTerm(x,this))
    case _ => None
  }

  def getType(t:Term):Term = termType;

  val theory: Theory = th;

  lazy val termType = th.freeAtomSignature.createConstant("BigInteger").get;

}
