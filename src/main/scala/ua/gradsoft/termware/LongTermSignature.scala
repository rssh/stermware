package ua.gradsoft.termware;


/**
 * Signature for long
 */
class LongTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Long => LongTerm(x,this)
    case _ => throwUOE
  }

  val typeName = "Long" ;

  /**
   * native Long object reference.
   **/
  def toAnyRef(t:Term) = java.lang.Long.valueOf(t.getLong);

  /**
   * return long value.
   **/
  def toAny(t:Term) = t.long_! ;




}
