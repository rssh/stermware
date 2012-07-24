package ua.gradsoft.termware.util;

import ua.gradsoft.termware.Term;

/**
 * Covariant ordering.
 **/
class TermOrdering[A<:Term] extends Ordering[A] 
{
  def compare(x:A, y:A) = x.compare(y);
}

