package ua.gradsoft.termware;

class AtomTermSignature(th:Theory, tn:Name) extends TermSignature
{

  override def fixedName = None;

  override def fixedArity = Some(0);

  override def nameByIndex = None;
  override def indexByName = None;

  override def createTerm(name:Name, args: RandomAccessSeq[Term]) = None;
  override def createSpecial(args: Any*) = None;


  def createConstant(arg:Any) = arg match {
     case x: String => Some(new AtomTerm(
                          TermWare.instance.symbolTable.getOrCreate(x),
                          this))
     case x: Name => Some(new AtomTerm(x,this))
     case _ => None
  };

  /**
   * type of atom is atom itself.
   **/
  override def getType(t:Term):Term = t;

  val theory: Theory = th;

}

