package ua.gradsoft.termware.fn;

import ua.gradsoft.termware.VM;

case class FnUpToMarker(marker: Int) extends Function1[VM,VM]
{
  override def apply(vm:VM):VM = {
    var quit=false;
    while(!quit) {
      vm.popCommand match {
        case x: FnMarker => quit = (x.marker == marker) ;
        case _ => // do nothing
      }
    }
    vm;
  }
}
