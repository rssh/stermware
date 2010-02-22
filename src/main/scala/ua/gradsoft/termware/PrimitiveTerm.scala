package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;

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

  def subterms = RandomAccessSeq.empty;

  override def isX = false;

  override def isEta = false;

  override def isError = false;

  override def message = None;

  def termSubst(s:PartialFunction[Term,Term]):Term = 
    if (s.isDefinedAt(this)) s.apply(this) else this;

  var attributes = new HashMap[Name,Term]();
}
