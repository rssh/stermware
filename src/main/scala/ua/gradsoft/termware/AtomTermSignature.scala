package ua.gradsoft.termware;

class AtomTermSignature(th:Theory, tn:Name) extends TermSignature
                                               with GeneralUtil
{

  override def fixedName = None;

  override def fixedArity = Some(0);

  override def nameByIndex = None;
  override def indexByName = None;

  override def createTerm(name:Name, args: RandomAccessSeq[Term]) = throwUOE;
  override def createSpecial(args: Any*) = throwUOE;


  def createConstant(arg:Any) = arg match {
     case x: String => new AtomTerm(
                          TermWare.instance.symbolTable.getOrCreate(x),
                          this)
     case x: Name => new AtomTerm(x,this)
     case _ => throwUOE
  };

  /**
   * type of atom is atom itself.
   **/
  override def getType(t:Term):Term = t;

  val theory: Theory = th;

}

