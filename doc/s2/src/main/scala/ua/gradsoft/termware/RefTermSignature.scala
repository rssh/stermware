package ua.gradsoft.termware;

/**
 * Signature for opaque object reference, incapsulated in term
 */
class RefTermSignature(th:Theory) extends PrimitiveTermSignature(th)
                                        with RefTranslatedTermSignature
{

  override def createConstant(arg:Any):Term = arg match {
          case x:AnyRef => new RefTerm(x,this);
          case _ => throwUOE;
  }

  def typeName="Ref";

  def toAnyRef(t:Term) = t.getRef;

  def fromAnyRef(x:AnyRef) =
    x match {
      case t: Term => if (t.isRef) Some(t) else Some(new RefTerm(t,this))
      case _ => Some(new RefTerm(x,this))
    }

}
