package ua.gradsoft.termware;

class NilTermSignature(th:Theory) extends PrimitiveTermSignature
{

  override def getType(t:Term):Term = nilType;
  
  override def createConstant(arg:Any) = Some(nil);

  val theory = th;
  val nil = new NilTerm(this);
  lazy val nilType:Term = theory.atomSignature("Nil").createConstant().get;

}
