package ua.gradsoft.termware;

/**
 * Signature for character
 */
class CharTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:Char => Some(new CharTerm(x,this))
    case _ => None
  }

  override def typeName = "Char" ;

}
