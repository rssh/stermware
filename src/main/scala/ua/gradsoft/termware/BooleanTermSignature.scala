package ua.gradsoft.termware;

/**
 * Signature for boolean
 */
class BooleanTermSignature(th:Theory) extends PrimitiveTermSignature
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:Boolean => Some(BooleanTerm(x,this))
    case _ => None
  }

  def getType(t:Term):Term = termType;

  val theory: Theory = th;

  lazy val termType = th.freeAtomSignature.createConstant("Boolean").get;

}
