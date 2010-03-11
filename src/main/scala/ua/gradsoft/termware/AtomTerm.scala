package ua.gradsoft.termware;


case class AtomTerm(n:Name,s:AtomTermSignature) extends PrimitiveTerm(s)
                                            with NonNumberTerm
{

  override def isAtom = true;

  override def isNil = false;
  
  def termClassIndex=TermClassIndex.ATOM;

  def termCompare(t:Term):Int = {
    val cl = termClassIndex - t.termClassIndex;
    if (cl != 0) return cl;
    return name.compareTo(t.name); 
  }

  def termHashCode: Int = 1+name.hashCode;

  override def toString = name.toString;

  val name = n;
}

