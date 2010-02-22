package ua.gradsoft.termware.fn;

import ua.gradsoft.termware.VM;

object FnIfPop
{

 def mk(inIfTrue:VM=>VM, 
        inIfFalse:VM=>VM):VM=>VM = FnIfPop(inIfTrue,inIfFalse);

};


case class FnIfPop(inIfTrue:VM=>VM,inIfFalse:VM=>VM) extends Function1[VM,VM]
{

  def apply(vm:VM):VM = {
    val b:Boolean = vm.popData.asInstanceOf[Boolean];
    vm.pushCommand (if (b) ifTrue else ifFalse);
    return vm;
  }

  val ifTrue = inIfTrue;
  val ifFalse = inIfFalse;
}
