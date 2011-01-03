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

   def termType(cbt:ComputationBounds[Term])(implicit ctx:CallContext):
                                                    ComputationBounds[Term]=
      CallCC.compose(cbt,{ (t:Term,ctx:CallContext)=>Done(simpleTermType(t))});

   def simpleTermType(t:Term):Term;

}
