package ua.gradsoft.termware;

trait FreeAlgebra extends Theory
{

  val booleanSignature = new BooleanTermSignature(this);
  val TRUE = booleanSignature.createConstant(true);
  val FALSE = booleanSignature.createConstant(false);
  val atomSignature = new AtomTermSignature(this,
                         TermWare.symbolTable.getOrCreateElement("FREE_ATOM"));
  val nilSignature = new NilTermSignature(this);  

}
