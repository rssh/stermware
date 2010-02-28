package ua.gradsoft.termware;

class AtomTermSignature(th:Theory, tn:Name) extends PrimitiveTermSignature
{

  def getType(t:Term):Term = typeTerm;
  
  def createConstant(arg:Any) = arg match {
     case x: String => Some(new AtomTerm(
                          TermWare.instance.symbolTable.getOrCreate(x),
                          this))
     case x: Name => Some(new AtomTerm(x,this))
     case _ => None
  };


  val theory = th;
  val typeTerm = new AtomTerm(tn,this);

}
