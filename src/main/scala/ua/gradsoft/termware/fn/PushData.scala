package ua.gradsoft.termware.fn;

import ua.gradsoft.termware.VM;

case class PushData(a:Any) extends Function1[VM,VM]
{
  def apply(vm:VM):VM = {
    vm.pushData(a);
    return vm;
  }
  val data=a
}


