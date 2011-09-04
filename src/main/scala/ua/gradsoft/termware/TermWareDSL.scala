package ua.gradsoft.termware;

trait TermWareDSL extends DefaultTermNames
{

  def theory: Theory;

  implicit def intToTerm(x:Int):Term = theory.intSignature.createConstant(x);
  implicit def longToTerm(x:Long):Term = theory.longSignature.createConstant(x);
  implicit def stringToTerm(x:Long):Term = theory.stringSignature.createConstant(x);

  /**
   *Functionl term represented as
   * Name :: (x1,...xN) 
   *  or
   * Name :: Seq(  )
   **/

  class NameVerb(fn:Name)
  {
     def with_ (x:Seq[Term]): Term = theory.funSignature(fn).createTerm(fn,x:_*); 
     def `with` (x:Term*): Term = theory.funSignature(fn).createTerm(fn,x:_*); 
     def * (x:Term*): Term = theory.funSignature(fn).createTerm(fn,x:_*); 
  };

  def FN(fn:Name) = new NameVerb(fn);
  def FN(fn:String) = new NameVerb(theory.symbolTable.getOrCreate(fn));
  def <>(fn:Name) = new NameVerb(fn);
  def <>(fn:String) =  new NameVerb(theory.symbolTable.getOrCreate(fn));



}

object TermWareDSL extends TermWareDSL
{

  def theory = TermWare.instance.freeTheory;

}