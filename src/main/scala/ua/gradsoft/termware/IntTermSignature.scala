package ua.gradsoft.termware;


/**
 * Signature for int
 */
class IntTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:Int => Some(IntTerm(x,this))
    case _ => None
  }

  override def typeName = "Int" ;

}
