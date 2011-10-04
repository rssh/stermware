package ua.gradsoft.termware;

/**
 * Signature for string
 */
class StringTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:String => new StringTerm(x,this)
    case _ => throwUOE;
  }

  override def typeName = "String";

  def toAnyRef(t:Term) = t.getString;

  def toAny(t:Term) = t.getString ;


}
