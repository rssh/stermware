package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

import java.io.PrintWriter;
import java.io.OutputStream;
import java.io.StringWriter;

/**
 * Uniform representation of term object.
 **/
trait Term extends TValue 
        with Ordered[Term]
        with TermAttributed
        with GeneralUtil
        with Serializable
{

  def arity: Int;

  def patternArity: Option[Int] = Some(arity);

  def subterm(i:Int): Term;

  def subterms: IndexedSeq[Term];

  def name: Name;

  def patternName: Option[Name] = Some(name);

  def signature: TermSignature;

  def theory: Theory = signature.theory;

  def termType(implicit ctx:CallContext)
                       = signature.termType(Done(this))(ctx);

  def isX: Boolean = false;

  def xOwner: Term =  throwUOE;

  def xLabel: Int = 0;

  def isNil: Boolean;

  def isAtom: Boolean;

  def isEta: Boolean;

  def isError: Boolean;

  def subst(s: PartialFunction[Term,Term])(implicit ctx:CallContext): 
                                                     ComputationBounds[Term];

  def fixSubst(s: PartialFunction[Term,Term]): Term;

  def unify(t:Term, s: Substitution)(implicit ctx:CallContext):
                                  ComputationBounds[(Boolean,Substitution)];

  def onUnify[T](t:Term, s:Substitution)
         (cont:((Boolean,Substitution),CallContext)=>ComputationBounds[T])
         (implicit ctx:CallContext) =
  {
     val f = Call{ (ctx:CallContext) => unify(t,s)(ctx); };
     CallCC.compose(f,cont);
  };

  def fixUnify(t:Term, s:Substitution): (Boolean, Substitution);

  def termClassIndex: Int; 

  def fixTermCompare(t:Term): Int; 

  def termCompare(t:Term)(implicit ctx:CallContext):ComputationBounds[Int]

  def onTermCompare[T](t:Term)
                      (cont:(Int,CallContext) => ComputationBounds[T]) 
                      (implicit ctx:CallContext) : ComputationBounds[T] =
     CallCC.compose(termCompare(t),cont);

  override def compare(that:Term):Int = fixTermCompare(that);

  def termEq(t:Term)(implicit ctx:CallContext): ComputationBounds[Boolean] = {
    CallCC.compose(termCompare(t),
                   { (x:Int) => Done(x==0) });
  }

  def fixTermEq(t:Term): Boolean;

  def termHashCode: Int; 

  override def hashCode = termHashCode;

  def print(out: PrintWriter): Unit ;

  def print(out: OutputStream = System.out): Unit = {
    val writer = new PrintWriter(out);
    print(writer);
    writer.flush();
  }

  def sprint: String = {
    val sw = new StringWriter();
    val pw = new PrintWriter(sw);
    print(pw);
    pw.flush();
    return sw.toString
  }

}

