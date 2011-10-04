package ua.gradsoft.termware;

/**
 * Signature for opaque object reference, incapsulated in term
 */
class RefTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
          case x:AnyRef => new RefTerm(x,this);
          case _ => throwUOE;
  }

  def typeName="Ref";

  def toAnyRef(t:Term) = t.getRef;

  def toAny(t:Term) = t.getRef;


}
