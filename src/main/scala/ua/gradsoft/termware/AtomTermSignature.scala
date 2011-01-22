package ua.gradsoft.termware;

class AtomTermSignature(override val theory:Theory, tn:Name) 
                                              extends SimpleTermSignature
                                               with GeneralUtil
{

  override def fixedName = None;

  override def createTerm(name:Name, args: IndexedSeq[Term]) = throwUOE;
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
  override def simpleTermType(t:Term):Term = t;

}

