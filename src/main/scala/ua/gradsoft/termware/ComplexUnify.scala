package ua.gradsoft.termware;


trait ComplexUnify extends Term
{

  override def termUnify(t:Term, s:Substitution, vm: VM): 
                                         (Boolean, Substitution) = {
     val marker = vm.createAndPushMarker;
     vm.pushCommand(termUnifyFn(t,s));
     vm.runByMarker(marker);
     val retval: Boolean = vm.popData.asInstanceOf[Boolean];
     if (retval) {
        return (retval,vm.popData.asInstanceOf[Substitution]); 
     } else {
        return (retval,s);
     }
  }

  def termUnify(t:Term, s:Substitution): (Boolean, Substitution) = {
     val vm = new VM;
     return termUnify(t,s,vm);
  }

}
