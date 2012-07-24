package ua.gradsoft.termware;

class AtomTermSignature(override val theory:Theory, tn:Name) 
                                              extends SimpleTermSignature
                                               with GeneralUtil
                                               with RefTranslatedTermSignature
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
  override def termType(t:Term):Term = t;

  /**
   * reference of atom is atom itself.
   **/
  def toAnyRef(t:Term) = t;

  def fromAnyRef(x:AnyRef) =
   x match {
      case t: Term => if (t.isAtom) Some(t) else None
      case _ => None
   }


}

