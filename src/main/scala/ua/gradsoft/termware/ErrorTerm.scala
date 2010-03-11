package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import ua.gradsoft.termware.fn.FnNone;

class ErrorTerm(m:String, e:Exception, s: ErrorTermSignature) extends Term
                                          with SimpleSubst
                                          with SimpleUnifyWithoutVM
                                          with NonNumberTerm
{

  def this(e:Exception, s:ErrorTermSignature) = {
    this(e.getMessage(),e,s);
  }

  def this(m:String, s:ErrorTermSignature) = {
    this(m,null,s);
  }


  def arity: Int = 0;

  def subterm(i:Int): Term = throwUOE;

  def subterms: RandomAccessSeq[Term] = RandomAccessSeq.empty;

  def name: Name = signature.theory.symbolTable.ERROR;

  val signature: TermSignature = s;

  def isNil: Boolean = false;

  def isAtom: Boolean = false;

  def isEta: Boolean = false;

  def isError: Boolean = true;

  override def isException: Boolean = true;

  override def getException: Exception = exception;

  override def getMessage: String = message;

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
     return message.compareTo(t.getMessage);
  }

  def termHashCode: Int = 7+message.hashCode;

  val message=m;
  val exception=e;

  val attributes = new HashMap[Name,Term](); 
}
