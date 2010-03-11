package ua.gradsoft.termware;


/**
 * Signature for int
 */
class IntTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Int => IntTerm(x,this)
    case _ => throwUOE;
  }

  override def typeName = "Int" ;

}
