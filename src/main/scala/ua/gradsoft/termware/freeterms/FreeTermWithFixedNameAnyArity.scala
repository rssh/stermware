package ua.gradsoft.termware.freeterms;

import ua.gradsoft.termware._;

class FreeTermWithFixedNameAnyArity(
                         ts:RandomAccessSeq[Term],
                         s:TermSignature) extends FunctionalTerm(s)
                                        with
                                          FixedNameTerm
{

  def arity: Int = subterms.length;

  def subterm(i:Int):Option[Term] =
    if (i<subterms.length) Some(subterms(i)) else None

  lazy val termHashCode = name.hashCode*7+subterms.
                              foldLeft(0)((x:Int,y:Term)=>x+y.termHashCode);

  val subterms = ts;
}

