package ua.gradsoft.termware;


@serializable
trait Term extends TValue 
        with Ordered[Term]
{

  def arity: Int;

  def subterm(i:Int): Option[Term];

  def subterms(): Seq[Term];

  def name: Name;

  def patternName: Name = name;

  //def termType: Term;

  def signature: TermSignature;

  def isX: Boolean = false;

  def xOwner: Option[Term] = None; 

  def xLabel: Int = 0;

  def isNil: Boolean;

  def isAtom: Boolean;

  def isEta: Boolean;

  def termSubstFn(s: PartialFunction[Term,Term]): (VM=>VM) ;

  def termSubst(s: PartialFunction[Term,Term], vm: VM):Term;

  def termSubst(s: PartialFunction[Term,Term]): Term;

  def termUnifyFn(t:Term, s: Substitution): (VM=>VM) ;

  def termUnify(t:Term, s:Substitution, vm: VM): (Boolean, Substitution);

  def termUnify(t:Term, s:Substitution): (Boolean, Substitution);

  def termClassIndex: Int; 

  def termCompare(t:Term): Int; 

  def compare(that:Term):Int = termCompare(that);

  def termHashCode: Int; 

  override def hashCode = termHashCode;

}

