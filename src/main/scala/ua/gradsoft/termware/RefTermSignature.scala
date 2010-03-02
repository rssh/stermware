package ua.gradsoft.termware;

/**
 * Signature for opaque object reference, incapsulated in term
 */
class RefTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Option[Term] = arg match {
          case x:AnyRef => Some(new RefTerm(x,this));
          case _ => None;
  }

  def typeName="Ref";

}
