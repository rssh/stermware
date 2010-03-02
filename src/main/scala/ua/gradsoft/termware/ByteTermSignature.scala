package ua.gradsoft.termware;


/**
 * Signature for short
 */
class ByteTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:Byte => Some(ByteTerm(x,this))
    case _ => None
  }

  override def typeName = "Byte" ;

}


