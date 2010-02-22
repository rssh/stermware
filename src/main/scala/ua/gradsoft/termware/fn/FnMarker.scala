package ua.gradsoft.termware.fn;

import ua.gradsoft.termware.VM;

case class FnMarker(m: Integer) extends Function1[VM,VM]
{
  override def apply(vm:VM):VM = vm;
  val marker:Integer=m;
}
