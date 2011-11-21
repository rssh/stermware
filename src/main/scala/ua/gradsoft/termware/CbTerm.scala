package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

import java.io.PrintWriter;
import java.io.OutputStream;
import java.io.StringWriter;

/**
 * Uniform representation of term object, which can not
 * be yet fully computated as set of arrows, which 
 * produce partial results.
 **/
trait CbTerm extends CbTValue 
        with Unificable[CbTerm]
        with TermAttributed
        with GeneralUtil
        with Serializable
{

  def arity(implicit ctx:CallContext): ComputationBounds[Int];

  def patternArity(implicit ctx: CallContext):ComputationBounds[Option[Int]] = 
       CallCC.some(arity);

  def subterm(i:Int)(implicit ctx:CallContext): CbTerm;

  def subterms(implicit ctx:CallContext): ComputationBounds[IndexedSeq[CbTerm]];

  def name(implicit ctx:CallContext): ComputationBounds[Name];

  def patternName(implicit ctx:CallContext): ComputationBounds[Option[Name]] = 
       CallCC.some(name);

  def signature(implicit ctx:CallContext): ComputationBounds[TermSignature];

  def theory(implicit ctx:CallContext): ComputationBounds[Theory];

  def isX(implicit ctx:CallContext): ComputationBounds[Boolean];

  def xOwner(implicit ctx:CallContext): ComputationBounds[XOwner] ;

  def xLabel(implicit ctx:CallContext): ComputationBounds[Int];

  def isNil(implicit ctx:CallContext): ComputationBounds[Boolean];

  def isAtom(implicit ctx:CallContext): ComputationBounds[Boolean];

  def isEta(implicit ctx:CallContext): ComputationBounds[Boolean];

  def isError(implicit ctx:CallContext): ComputationBounds[Boolean];

  def subst(s: PartialFunction[CbTerm,CbTerm])(implicit ctx:CallContext): 
                                               ComputationBounds[CbTerm];

  def unify(t:CbTerm, s: Substitution[CbTerm])(implicit ctx:CallContext):
                                ComputationBounds[(Boolean,Substitution[CbTerm])];

  def termClassIndex(implicit ctx:CallContext): ComputationBounds[Int]; 

  def termCompare(t:CbTerm)(implicit ctx:CallContext):ComputationBounds[Int]

  def onTermCompare[T](t:CbTerm)
                      (cont:(Int,CallContext) => ComputationBounds[T]) 
                      (implicit ctx:CallContext,mt:Manifest[T]) : ComputationBounds[T] =
  {
   CallCC.compose(termCompare(t),cont);
  }

  def termEq(t:CbTerm)(implicit ctx:CallContext): ComputationBounds[Boolean] = {
    CallCC.compose(termCompare(t),
                   { (x:Int) => Done(x==0) });
  }

  def toTerm(implicit ctx:CallContext): Term = new CbTermTerm(this,ctx);

  def toTermComputationBounds: ComputationBounds[Term] =
       Call{ (ctx:CallContext) => Done(toTerm(ctx)) }

  def print(out:PrintWriter):Unit;

}



