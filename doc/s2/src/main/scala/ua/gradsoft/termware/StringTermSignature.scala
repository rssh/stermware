package ua.gradsoft.termware;

/**
 * Signature for string
 */
class StringTermSignature(th:Theory) extends PrimitiveTermSignature(th)
                                       with RefTranslatedTermSignature
{

  override def createConstant(arg:Any):Term = arg match {
    case x:String => new StringTerm(x,this)
    case _ => throwUOE;
  }

  override def typeName = "String";

  def toAnyRef(t:Term) = t.getString;

  def fromAnyRef(x:AnyRef) =
   x match {
      case  s: String => Some(new StringTerm(s,this))
      case  t: Term => if (t.isString) Some(t) else None
      case _ => None
   }


}
