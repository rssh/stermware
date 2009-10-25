package ua.gradsoft.termware;

trait FreeAlgebra extends Theory
{

  override def getBooleanSignature: TermSignature = booleanSignature;


  val booleanSignature = new BooleanTermSignature(this);
  val TRUE = booleanSignature.createConstant(true);
  val FALSE = booleanSignature.createConstant(false);
}
