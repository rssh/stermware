package ua.gradsoft.termware.fn;

import ua.gradsoft.termware.VM;

object FnTrue extends FnTrue
{}

case class FnTrue extends Function1[VM,VM]
{
   def apply(vm:VM):VM = { vm.pushData(true); return vm; }
}

