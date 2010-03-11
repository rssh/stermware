package ua.gradsoft.termware;

/**
 * Signature for character
 */
class CharTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Char => new CharTerm(x,this)
    case _ => throwUOE;
  }

  override def typeName = "Char" ;

}
