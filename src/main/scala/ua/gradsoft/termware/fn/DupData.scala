package ua.gradsoft.termware.fn;

import ua.gradsoft.termware.VM;

class DupData extends Function1[VM,VM]
{
  def apply(vm:VM):VM = {
    val a = vm.popData;
    vm.pushData(a);
    vm.pushData(a);
    return vm;
  }
}

object DupData extends DupData {}

