package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import java.io.PrintWriter;

/**
 * trait for primitive terms.
 */
abstract class PrimitiveTerm(s:TermSignature) extends Term
                                      with SimpleUnifyWithoutVM
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

  def termSubst(s:PartialFunction[Term,Term]):Term = 
    if (s.isDefinedAt(this)) s.apply(this) else this;

  def termUnify(t: Term, s: Substitution)
   =
     if (termEq(t))
            (true,s)
     else
       if (t.isX) {
         val r = s+(t->this);
         (r._1, if (r._1) r._2 else s);
       } else
         (false, s)
   ;

  override def print(out:PrintWriter):Unit = out.print(toString);

  val signature = s;
  lazy val attributes = new HashMap[Name,Term]();

}
