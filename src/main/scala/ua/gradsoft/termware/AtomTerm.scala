package ua.gradsoft.termware;


class AtomTerm(n:Name,s:AtomTermSignature) extends PrimitiveTerm
{

  def isAtom = true;

  def isNil = false;
  
  def termUnify(t:Term, s: Substitution) =
   if (t.isAtom)
     if (t.name.compareTo(name)==0) (true,s)
       else (false,s)
    else 
     if (t.isX)
        s+(t->this)
     else
        (false,s);
          
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

