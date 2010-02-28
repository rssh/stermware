package ua.gradsoft.termware;


class NilTerm(s:NilTermSignature) extends PrimitiveTerm(s)
                                    with FixedNameTerm
                                    with NonNumberTerm
{

  override def isNil = true;

  override def isAtom = false;
  
  def termClassIndex=TermClassIndex.NIL;

  def termCompare(t:Term):Int = 
    termClassIndex - t.termClassIndex;

  def termHashCode: Int = 1+name.hashCode;

}

