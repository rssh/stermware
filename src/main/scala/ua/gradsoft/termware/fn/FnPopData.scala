package ua.gradsoft.termware.fn;

import ua.gradsoft.termware.VM;

case class FnPopData() extends Function1[VM,VM]
{
  def apply(vm:VM):VM = {
    val a = vm.popData;
    return vm;
  }
};

object FnPopData extends FnPopData()
{
};


