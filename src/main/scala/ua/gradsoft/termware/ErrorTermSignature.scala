package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

class ErrorTermSignature(th:Theory) extends TermSignature
                                       with GeneralUtil
{

  override def termType(t:Term): Term =
   typeTerm;

  override def fixedName = Some(theory.symbolTable.ERROR);

  override def fixedArity = Some(0);

  override def nameByIndex = None;
  override def indexByName = None;

  override def createTerm(name:Name, args: IndexedSeq[Term]) = throwUOE;
  override def createSpecial(args: Any*) = throwUOE;

  override def createConstant(arg:Any) = arg match {
     case s: String => new ErrorTerm(s, this)
     case e: Exception => new ErrorTerm(e, this)
     case _ => throwUOE;
  };

  /**
   * return original exception
   **/
  def toAnyRef(t:Term) = t.getException;

  def toAny(t:Term) = t.getException;

  lazy val typeTerm = theory.atomSignature(theory.symbolTable.ERROR).createConstant(theory.symbolTable.ERROR);
  val theory = th;


};
