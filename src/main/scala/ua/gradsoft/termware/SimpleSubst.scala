package ua.gradsoft.termware;

import ua.gradsoft.termware.vm.VM;

trait SimpleSubst extends Term
{

  override def termSubst(s:Substitution, vm: VM) 
    = 
     Right(termSubst(s));

}
