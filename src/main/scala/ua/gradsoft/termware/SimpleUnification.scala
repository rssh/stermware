package ua.gradsoft.termware;

trait SimpleUnification extends Term
{

  override def termUnifyFn(t:Term, s: Substitution): VM => VM
    =  ( vm : VM ) => { 
        val v = termUnify(t,s); 
        vm.pushData(v._2); 
        vm.pushData(v._1); 
        vm; 
  }

  override def termUnify(t:Term, s:Substitution, vm: VM) 
    =  termUnify(t,s);
    

}
