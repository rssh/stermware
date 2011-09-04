package ua.gradsoft.termware;

import scala.collection.mutable.{HashMap => MutableHashMap};
import java.io.PrintWriter;
import ua.gradsoft.termware.flow._;


class ErrorTerm(m:String, e:Exception, s: ErrorTermSignature) extends Term
                                          with SimpleSubst
                                          with SimpleUnify
                                          with NonNumberTerm
                                          with NonBooleanTerm
{

  def this(e:Exception, s:ErrorTermSignature) = {
    this(e.getMessage(),e,s);
  }

  def this(m:String, s:ErrorTermSignature) = {
    this(m,null,s);
  }


  def arity: Int = 0;

  def subterm(i:Int): Term = throwUOE;

  def subterms: IndexedSeq[Term] = IndexedSeq.empty;

  def name: Name = signature.theory.symbolTable.ERROR;

  val signature: TermSignature = s;

  def isNil: Boolean = false;

  def isAtom: Boolean = false;

  def isEta: Boolean = false;

  def isError: Boolean = true;

  override def isException: Boolean = true;

  override def getException: Exception = exception;

  override def getMessage: String = message;

  def fixSubst(s: PartialFunction[Term,Term]): Term = this;

  def fixUnify(t:Term, s:Substitution[Term]): (Boolean, Substitution[Term]) = (false, s);
   
  def termClassIndex: Int = TermClassIndex.ERROR;

  def fixTermEq(t:Term):Boolean = t.isError && message == t.getMessage;

  def fixTermCompare(t:Term): Int = 
  {
   val cl = termClassIndex - t.termClassIndex;
   if (cl!=0) 
     return cl;
   else
     return message.compareTo(t.getMessage);
  }

  def termCompare(t:Term)(implicit ctx:CallContext):ComputationBounds[Int]
   = Done(fixTermCompare(t));



  def termHashCode: Int = 7+message.hashCode;

  override def print(out: PrintWriter) = { out.format("Error(%s)",message); }

  val message=m;
  val exception=e;

  val attributes = new MutableHashMap[Name,Term](); 


}
