package ua.gradsoft.termware;

trait TermSystem
{

  def reduceFn:(VM=>VM);
  def theory: Theory;

  def reduce(t:Term,vm:VM): Term = {
    val marker = vm.createAndPushMarker;
    vm.pushCommand(reduceFn);
    vm.pushData(t);
    vm.runByMarker(marker);
    vm.popData match {
       case r: Term => r
       case _ => theory.errorSignature.createSpecial("term expected",vm)
   }
  }

  def reduce(t:Term):Term = reduce(t, new VM());


}
