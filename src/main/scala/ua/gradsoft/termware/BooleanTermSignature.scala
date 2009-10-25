package ua.gradsoft.termware;

class BooleanTermSignature(th:Theory) extends TermSignature
{

  override def isFixedName = false;
  override def getFixedName = None;

  override def isFixedArity = true;
  override def getFixedArity = Some(0);

  override def isConcrete = true;

  override def isNamedSubterms = false;
  override def getNameByIndex(index:Int) = None;
  override def getIndexByName(name:Name) = None;

  override def createConstant(arg:Any):Option[Term] = arg match {
    case x:Boolean => Some(new BooleanTerm(x,this))
    case _ => None
  }

  val theory: Theory = th;

}
