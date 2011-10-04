package ua.gradsoft.termware;

class NilTermSignature(th:Theory) extends PrimitiveTermSignature(th)
{

  override def fixedName:Option[Name] = Some(NIL);

  override def createConstant(arg:Any) = nil;
  override def createSpecial(arg:Any*) = nil;
  override def typeName = "Nil";

  val nil = new NilTerm(this);
  lazy val NIL = th.symbolTable.getOrCreate("Nil");

 /**
   * reference of nil is scala nil.
   **/
  def toAnyRef(t:Term) = Nil;

  /**
   * return scala nil.
   **/
  def toAny(t:Term) = Nil;


}
