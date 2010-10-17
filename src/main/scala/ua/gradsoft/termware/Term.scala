package ua.gradsoft.termware;

import java.io.PrintWriter;
import java.io.OutputStream;
import java.io.StringWriter;

@serializable
trait Term extends TValue 
        with Ordered[Term]
        with TermAttributed
        with GeneralUtil
{

  def arity: Int;

  def patternArity: Option[Int] = Some(arity);

  def subterm(i:Int): Term;

  def subterms: IndexedSeq[Term];

  def name: Name;

  def patternName: Option[Name] = Some(name);

  def signature: TermSignature;

  def theory: Theory = signature.theory;

  def isX: Boolean = false;

  def xOwner: Term =  throwUOE;

  def xLabel: Int = 0;

  def isNil: Boolean;

  def isAtom: Boolean;

  def isEta: Boolean;

  def isError: Boolean;

  def termSubstFn(s: PartialFunction[Term,Term]): (VM=>VM) ;

  def termSubst(s: PartialFunction[Term,Term], vm: VM):Term;

  def termSubst(s: PartialFunction[Term,Term]): Term;

  /**
   * leave on stack result of unification.
   * (on top is true or false, later - substitution)
   * i.e.
   * after termUnifyFn(t,s) ; 
   *   (vm.popData , vm.popData) = termUnify(t,s)
   **/
  def termUnifyFn(t:Term, s: Substitution): (VM=>VM) ;

  def termUnify(t:Term, s:Substitution, vm: VM): (Boolean, Substitution);

  def termUnify(t:Term, s:Substitution): (Boolean, Substitution);

  def termClassIndex: Int; 

  def termCompare(t:Term): Int; 

  override def compare(that:Term):Int = termCompare(that);

  def termEq(t:Term): Boolean = (termCompare(t)==0);

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

