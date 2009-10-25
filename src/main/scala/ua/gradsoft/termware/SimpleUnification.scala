package ua.gradsoft.termware;

import ua.gradsoft.termware.vm.VM;

trait SimpleUnification extends Term
{

  override def termUnify(t:Term, s:Substitution, vm: VM) 
    = 
     Right(termUnify(t,s));

}
