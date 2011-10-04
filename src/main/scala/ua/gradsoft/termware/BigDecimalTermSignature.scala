package ua.gradsoft.termware;


/**
 * Signature for big decimal
 */
class BigDecimalTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:BigDecimal => BigDecimalTerm(x,this)
    case _ => throwUOE
  }

  override def typeName = "BigDecimal" ;

  /**
   * native BigDecimal object.
   **/
  def toAnyRef(t:Term) = t.getBigDecimal;

  /**
   * return native BigDecimal object.
   **/
  def toAny(t:Term) = t.getBigDecimal;


}
