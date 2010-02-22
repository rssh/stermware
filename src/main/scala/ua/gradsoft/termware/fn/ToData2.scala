package ua.gradsoft.termware.fn;

import ua.gradsoft.termware.VM;

object ToData2 extends Function1[VM,VM]
{
  def apply(vm:VM):VM = {
    vm.pushData2(vm.popData);
    return vm;
  }
}


