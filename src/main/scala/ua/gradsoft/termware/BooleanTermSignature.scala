package ua.gradsoft.termware;

/**
 * Signature for boolean
 */
class BooleanTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Boolean => BooleanTerm(x,this)
    case _ => throwUOE;
  }

  override def typeName = "Boolean";

}
