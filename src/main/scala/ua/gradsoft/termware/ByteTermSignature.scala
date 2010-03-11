package ua.gradsoft.termware;


/**
 * Signature for short
 */
class ByteTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Term = arg match {
    case x:Byte => ByteTerm(x,this)
    case _ => throwUOE;
  }

  override def typeName = "Byte" ;

}


