package ua.gradsoft.termware;

/**
 * Signature for float
 */
class FloatTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Float => FloatTerm(x,this)
    case _ => throwUOE;
  }

  override def typeName = "Float" ;

}
