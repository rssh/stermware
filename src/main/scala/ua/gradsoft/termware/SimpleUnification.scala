package ua.gradsoft.termware;

trait SimpleUnification extends Term
{

  override def termUnifyFn(t:Term, s: Substitution): VM => VM
    =  ( vm : VM ) => { vm.pushData(termUnify(t,s)); vm; }

  override def termUnify(t:Term, s:Substitution, vm: VM) 
    =  termUnify(t,s);
    

}
