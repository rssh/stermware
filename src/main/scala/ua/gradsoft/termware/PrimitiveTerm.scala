package ua.gradsoft.termware;

/**
 * trait for primitive terms.
 */
trait PrimitiveTerm extends Term
                        with SimpleUnification
                        with SimpleSubst
{

  def arity: Int = 0;

  def subterm(i:Int) = None;

  def subterm(name:Name) = None;

  def subtermsIterator = Iterator.empty;

  def isX = false;

  def getXIndex = None;


  def termSubst(s:Substitution):Term = this;

}
