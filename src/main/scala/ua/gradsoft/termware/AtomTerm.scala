package ua.gradsoft.termware;


class AtomTerm(n:Name,s:AtomTermSignature) extends PrimitiveTerm
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

  val name = n;
  val signature = s;
}

