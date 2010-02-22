package ua.gradsoft.termware;

trait ComplexSubst extends Term
{

  override def termSubst(s: PartialFunction[Term,Term], vm: VM):Term={
          val marker = vm.createAndPushMarker;
          vm.pushCommand(termSubstFn(s));
          vm.runByMarker(marker);
          return vm.popData.asInstanceOf[Term];
  }

  override def termSubst(s: PartialFunction[Term,Term]): Term =
  {
     val vm = new VM;
     return termSubst(s,vm);
  }

}
