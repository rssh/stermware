package ua.gradsoft.termware;

import ua.gradsoft.termware.vm._;

trait SimpleSubst extends Term
{

  override def termSubstFn(s: PartialFunction[Term,Term]): VM => VM = 
      (vm: VM) => { vm.pushData(termSubst(s)); vm; }

  override def termSubst(s:PartialFunction[Term,Term], vm: VM): Term 
    = termSubst(s);

}
