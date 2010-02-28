package ua.gradsoft.termware;

/**
 * Signature for opaque object reference, incapsulated in term
 */
class RefTermSignature(th:Theory) extends PrimitiveTermSignature
{

  override def createConstant(arg:Any):Option[Term] = arg match {
          case x:AnyRef => Some(new RefTerm(x,this));
          case _ => None;
  }

  def getType(t:Term):Term = termType;

  val theory: Theory = th;

  lazy val termType = th.freeAtomSignature.createConstant("Reference").get;

}
