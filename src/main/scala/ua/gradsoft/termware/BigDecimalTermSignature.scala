package ua.gradsoft.termware;


/**
 * Signature for big decimal
 */
class BigDecimalTermSignature(th:Theory) extends PrimitiveTermSignature
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:BigDecimal => Some(BigDecimalTerm(x,this))
    case _ => None
  }

  def getType(t:Term):Term = termType;

  val theory: Theory = th;

  lazy val termType = th.freeAtomSignature.createConstant("BigDecimal").get;

}
