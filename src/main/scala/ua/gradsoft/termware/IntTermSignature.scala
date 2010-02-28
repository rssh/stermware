package ua.gradsoft.termware;


/**
 * Signature for int
 */
class IntTermSignature(th:Theory) extends PrimitiveTermSignature
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:Int => Some(IntTerm(x,this))
    case _ => None
  }

  def getType(t:Term):Term = termType;

  val theory: Theory = th;

  lazy val termType = th.freeAtomSignature.createConstant("BigInteger").get;

}
