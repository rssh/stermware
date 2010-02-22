package ua.gradsoft.termware;


@serializable
trait Term extends TValue 
        with Ordered[Term]
        with Attributed[Term]
{

  def arity: Int;

  def subterm(i:Int): Option[Term];

  def subterms: RandomAccessSeq[Term];

  def name: Name;

  def patternName: Name = name;
  //def matcher: termMatched

  def signature: TermSignature;

  def isX: Boolean = false;

  def xOwner: Option[Term] = None; 

  def xLabel: Int = 0;

  def isNil: Boolean;

  def isAtom: Boolean;

  def isEta: Boolean;

  def isError: Boolean;

  def message: Option[String] = None;

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

  def termHashCode: Int; 

  override def hashCode = termHashCode;

}

