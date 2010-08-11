package ua.gradsoft.termware;

trait SimpleTermCondition extends TermCondition
{

  /**
   * return function which eval condition and leave result on data stack.
   **/
  def evalConditionFn(s:Substitution):(VM=>VM) = 
          (vm:VM) => { vm.pushData(evalCondition(s)(vm)); vm; }


}
