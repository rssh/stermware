package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import java.io.PrintWriter;

import ua.gradsoft.termware.flow._;

/**
 * trait for primitive terms.
 */
abstract class PrimitiveTerm(s:TermSignature) extends Term
                                      with ComplexUnify
                                      with SimpleSubst
{

  def arity: Int = 0;

  def subterm(i:Int) = throwUOE;

  def subterm(name:Name) = throwUOE;

  def subterms = IndexedSeq.empty;

  override def isX = false;

  override def isAtom = false;

  override def isNil = false;

  override def isEta = false;

  override def isError = false;

  override def getMessage = throwUOE;

  def fixSubst(s:PartialFunction[Term,Term]):Term = 
    if (s.isDefinedAt(this)) s.apply(this) else this;

  def unify(t: Term, s: Substitution[Term])(implicit ctx:CallContext)
   =
     if (fixTermEq(t))
            Done((true,s))
     else
       if (t.isX) {
         s+(t->this);
       } else
         Done(false, s)
   ;

  def termCompare(t:Term)(implicit ctx:CallContext):ComputationBounds[Int]
   = Done(fixTermCompare(t));

  override def print(out:PrintWriter):Unit = out.print(toString);

  val signature = s;
  lazy val attributes = new HashMap[Name,ComputationBounds[Term]]();

}
