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

  def subterms = Seq.empty;

  override def isX = false;

  override def isEta = false;

  def termSubst(s:PartialFunction[Term,Term]):Term = 
    if (s.isDefinedAt(this)) s.apply(this) else this;

}
