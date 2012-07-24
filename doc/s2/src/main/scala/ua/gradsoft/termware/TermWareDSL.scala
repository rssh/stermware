package ua.gradsoft.termware;

trait TermWareDSL extends DefaultTermNames
{


  def theory: Theory;
  def symbolTable: SymbolTable = theory.symbolTable;

  implicit def intToTerm(x:Int):Term = theory.intSignature.createConstant(x);
  implicit def longToTerm(x:Long):Term = theory.longSignature.createConstant(x);
  implicit def doubleToTerm(x:Double):Term = theory.doubleSignature.createConstant(x);
  implicit def stringToTerm(x:Long):Term = theory.stringSignature.createConstant(x);

  /**
   *Functionl term represented as
   *<pre>
   * Name `with` (x1,...xN) 
   *  or
   * Name with_ Seq(  )
   *
   * '*' can be used instead `with`
   *</pre>
   **/
  class NameVerb(fn:Name)
  {
     def with_ (x:Seq[Term]): Term = theory.funSignature(fn).createTerm(fn,x:_*); 
     def `with` (x:Term*): Term = theory.funSignature(fn).createTerm(fn,x:_*); 
     def apply(x:Term*): Term = theory.funSignature(fn).createTerm(fn,x:_*); 
  };

  def FN(fn:Name) = new NameVerb(fn);
  def FN(fn:String) = new NameVerb(theory.symbolTable.getOrCreate(fn));
  def <>(fn:Name) = new NameVerb(fn);
  def <>(fn:String) =  new NameVerb(theory.symbolTable.getOrCreate(fn));
  def f_ (fn:String) =  <>(fn);

  def atom(n:String) = theory.atomSignature(n).createConstant(n);
  def atom(n:Name) = theory.atomSignature(n).createConstant(n);
  
  case class XVarVerb(val n:Name)
  {
     def <-- (t:Term) = new AssignmentVerb(n,t);
     def <~ (t:Term) = new AssignmentVerb(n,t);
     def := (t:Term) = new AssignmentVerb(n,t);
  }
  implicit def toAtom(xv: XVarVerb): Term = atom(xv.n);

  def x(n:Name) = new XVarVerb(n);
  def x(n:String) = new XVarVerb(theory.symbolTable.getOrCreate(n));

  class AssignmentVerb(val left:Name, val right:Term);

  class AssignmentLeftVerb(val a:AtomTerm)
  {
    def <-- (t:Term) = new AssignmentVerb(a.name,t);
    def <~ (t:Term) = new AssignmentVerb(a.name,t);
    def := (t:Term) = new AssignmentVerb(a.name,t);
  }
  implicit def toAssignmentLetfVerm(a:AtomTerm):AssignmentLeftVerb =
     new AssignmentLeftVerb(a);


  class LetVerb(val assignments: Seq[AssignmentVerb])
  {
    def apply(t:Term): LetTerm = createLetTerm(t);

    def createLetTerm(t:Term):LetTerm =
    {
      new LetTerm( (assignments map ( 
                      x => new TermBinding(x.left, TermBinding.EAGER, x.right)
                    )).toIndexedSeq,
                    t,
                    TermConstructorTransformParams(true),
                    theory.letSignature
                 );
    }

  }

  def let(a:AssignmentVerb*) = new LetVerb(a);

}

object TermWareDSL extends TermWareDSL
{

  def theory = TermWare.instance.freeTheory;

}
