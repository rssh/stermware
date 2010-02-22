package ua.gradsoft.termware;

class ErrorTermSignature(th:Theory) extends TermSignature
{

  override def getType(t:Term):Term = typeTerm;

  override def fixedName = Some(theory.symbolTable.ERROR);

  override def fixedArity = Some(0);

  override def nameByIndex = None;
  override def indexByName = None;

  override def createTerm(name:Name, args: RandomAccessSeq[Term]) = None;
  override def createSpecial(args: Any*) = None;

  override def createConstant(arg:Any) = arg match {
     case s: String => Some(new ErrorTerm(this, s))
     case _ => None
  };

  lazy val typeTerm = theory.atomSignature(theory.symbolTable.ERROR).createConstant(theory.symbolTable.ERROR).get;
  val theory = th;
};
