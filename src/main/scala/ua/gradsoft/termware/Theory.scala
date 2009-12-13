package ua.gradsoft.termware;

trait Theory extends TermWareInstance
{

 def booleanSignature: TermSignature;

 def nilSignature: TermSignature;

 def freeFunSignature: TermSignature;

 def freeAtomSignature: TermSignature;

 def typeAlgebra: TermSystem;

 def getAtomSignature(name:Name): TermSignature;

 def getFunSignature(name:Name, args:Seq[Term]): TermSignature;

}
