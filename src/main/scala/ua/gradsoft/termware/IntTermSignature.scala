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

  /**
   * native Int object reference.
   **/
  def toAnyRef(t:Term) = java.lang.Integer.valueOf(t.getInt);

  /**
   * return int value.
   **/
  def toAny(t:Term) = t.int_! ;


}
