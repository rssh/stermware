package ua.gradsoft.termware.fn;

import ua.gradsoft.termware.VM;

object FromData2 extends Function1[VM,VM]
{
  def apply(vm:VM):VM = {
     vm.pushData(vm.popData2);
     vm;
  }
}

