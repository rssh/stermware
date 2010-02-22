package ua.gradsoft.termware.fn;

import ua.gradsoft.termware.VM;

object FnNone extends FnNone
{}

case class FnNone extends Function1[VM,VM]
{
   def apply(vm:VM):VM = vm ;
}

