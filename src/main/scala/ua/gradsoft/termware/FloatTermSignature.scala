package ua.gradsoft.termware;

/**
 * Signature for float
 */
class FloatTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:Float => Some(FloatTerm(x,this))
    case _ => None
  }

  override def typeName = "Float" ;

}
