package ua.gradsoft.termware;

trait SimpleUnify extends Term
{

  override def termUnifyFn(t:Term, s: Substitution): VM => VM
    =  ( vm : VM ) => { 
        val v = termUnify(t,s,vm); 
        vm.pushData(v._2); 
        vm.pushData(v._1); 
        vm; 
  }

}
