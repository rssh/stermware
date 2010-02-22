package ua.gradsoft.termware.fn;

import ua.gradsoft.termware.VM;
import ua.gradsoft.termware.Term;
import ua.gradsoft.termware.Name;
import ua.gradsoft.termware.ErrorTerm;
import ua.gradsoft.termware.ErrorTermException;

case class SetAttributeFnTN_V(t:Term,n:Name) extends Function1[VM,VM]
{
  def apply(vm:VM):VM = {
    vm.popData match {
       case x:Term =>  t.setAttribute(n,t);
       case _ => throw new ErrorTermException(
                             "argument for setAttributeFnTN_V must be term"
                                             );
    }
    return vm;
  }
}


