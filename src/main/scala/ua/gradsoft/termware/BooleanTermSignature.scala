package ua.gradsoft.termware;

/**
 * Signature for boolean
 */
class BooleanTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:Boolean => Some(BooleanTerm(x,this))
    case _ => None
  }

  override def typeName = "Boolean";

}
