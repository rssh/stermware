package ua.gradsoft.termware;


class NilTerm(s:NilTermSignature) extends PrimitiveTerm
                                    with FixedNameTerm
{

  def isNil = true;

  def isAtom = false;
  
  def termUnify(t:Term, s: Substitution) =
   if (t.isNil) 
     (true,s)
   else if (t.isX) 
     (s+(t->this))
   else
     (false,s); 
          
  def termClassIndex=TermClassIndex.NIL;

  def termCompare(t:Term):Int = 
    termClassIndex - t.termClassIndex;

  def termHashCode: Int = 1+name.hashCode;

  val signature = s;
}

