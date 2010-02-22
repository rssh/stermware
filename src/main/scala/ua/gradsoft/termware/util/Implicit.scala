package ua.gradsoft.termware.util;

import ua.gradsoft.termware.Term;

object Implicit
{

  implicit def toTOrdering[A<:Term] = new TermOrdering[A];

}



