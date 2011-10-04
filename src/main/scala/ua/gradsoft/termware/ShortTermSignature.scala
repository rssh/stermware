package ua.gradsoft.termware;


/**
 * Signature for short
 */
class ShortTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Short => ShortTerm(x,this)
    case _ => throwUOE;
  }

  def typeName = "Short" ;

  /**
   * native Short object reference.
   **/
  def toAnyRef(t:Term) = java.lang.Short.valueOf(t.getShort);

  /**
   * return short value.
   **/
  def toAny(t:Term) = t.short_! ;


}
