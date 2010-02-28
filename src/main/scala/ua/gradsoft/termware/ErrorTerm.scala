package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import ua.gradsoft.termware.fn.FnNone;

class ErrorTerm(s: ErrorTermSignature, m:String) extends Term
                                          with SimpleSubst
                                          with SimpleUnifyWithoutVM
                                          with NonNumberTerm
{

  def arity: Int = 0;

  def subterm(i:Int): Option[Term] = None;

  def subterms: RandomAccessSeq[Term] = RandomAccessSeq.empty;

  def name: Name = signature.theory.symbolTable.ERROR;

  val signature: TermSignature = s;

  def isNil: Boolean = false;

  def isAtom: Boolean = false;

  def isEta: Boolean = false;

  def isError: Boolean = true;

  override def termSubstFn(s: PartialFunction[Term,Term]): (VM=>VM) 
   = FnNone;

  def termSubst(s: PartialFunction[Term,Term]): Term = this;

  def termUnify(t:Term, s:Substitution): (Boolean, Substitution) = (false, s);
   
  def termClassIndex: Int = TermClassIndex.ERROR;

  def termCompare(t:Term): Int = 
  {
   val cl = termClassIndex - t.termClassIndex;
   if (cl!=0) 
     return cl;
   else
     return message.get.compareTo(t.message.get);
  }

  def termHashCode: Int = 7+message.hashCode;

  override val message=Some(m);

  val attributes = new HashMap[Name,Term](); 
}
