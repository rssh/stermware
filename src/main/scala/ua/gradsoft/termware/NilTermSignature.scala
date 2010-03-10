package ua.gradsoft.termware;

class NilTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def fixedName:Option[Name] = Some(NIL);

  override def createConstant(arg:Any) = Some(nil);
  override def typeName = "Nil";

  val nil = new NilTerm(this);
  lazy val NIL = th.symbolTable.getOrCreate("Nil");

}
