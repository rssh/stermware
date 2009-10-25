package ua.gradsoft.termware;

import ua.gradsoft.termware.vm.VM;

trait Term extends TValue
{

  def arity: Int;

  def subterm(i:Int): Option[Term];

  def subterm(name:Name): Option[Term];

  def subtermsIterator: Iterator[Term];

  //def termType: Term;

  //def termSignature: TermSignature;

  def isX: Boolean;

  def getXIndex: Option[Int];

  def termSubst(s: Substitution, vm: VM): Either[VM,Term];

  def termSubst(s: Substitution): Term;

  def termUnify(t:Term, s:Substitution, vm: VM): Either[VM,Option[Substitution]];
  def termUnify(t:Term, s:Substitution): Option[Substitution];

}

