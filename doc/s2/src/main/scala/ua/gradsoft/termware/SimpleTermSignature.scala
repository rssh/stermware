package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

trait SimpleTermSignature extends TermSignature
{

  def fixedArity: Option[Int] = Some(0);

  def indexByName: Option[Name => Option[Int]] = None;

  def nameByIndex: Option[Int => Option[Name]] = None;

  //def  fixedName: Option[Name];

  // def createTerm(name:Name, args: IndexedSeq[Term]): Term;

  // def createConstant(arg:Any): Term;
    
  // def createSpecial(args: Any*): Term;

   def termType(t:Term):Term;

}
