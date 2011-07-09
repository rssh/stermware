package ua.gradsoft.termware;


/**
 * term, which have bound variables.
 **/
abstract class XOwner(val vars:IndexedSeq[XTerm]) extends Term
{
  for(x <- vars) x.xOwner = this;


}
