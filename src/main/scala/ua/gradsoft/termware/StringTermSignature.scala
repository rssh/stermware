package ua.gradsoft.termware;

/**
 * Signature for string
 */
class StringTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:String => Some(new StringTerm(x,this))
    case _ => None
  }

  override def typeName = "String";

}
