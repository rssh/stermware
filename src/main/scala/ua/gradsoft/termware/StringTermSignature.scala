package ua.gradsoft.termware;

/**
 * Signature for string
 */
class StringTermSignature(th:Theory) extends PrimitiveTermSignature
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:String => Some(new StringTerm(x,this))
    case _ => None
  }

  def getType(t:Term):Term = termType;

  val theory: Theory = th;

  lazy val termType = th.freeAtomSignature.createConstant("String").get;

}
