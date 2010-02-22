package ua.gradsoft.termware.fn;

import ua.gradsoft.termware.VM;

object FnFalse extends FnFalse
{}

case class FnFalse extends Function1[VM,VM]
{
   def apply(vm:VM):VM = { vm.pushData(false); return vm; }
}

