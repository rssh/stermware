package ua.gradsoft.termware;


/**
 * Signature for big decimal
 */
class BigDecimalTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:BigDecimal => Some(BigDecimalTerm(x,this))
    case _ => None
  }

  override def typeName = "BigDecimal" ;

}
