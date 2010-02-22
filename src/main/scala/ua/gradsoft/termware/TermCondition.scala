package ua.gradsoft.termware;

trait TermCondition extends Term
{

  /**
   * return function which eval condition and leave result on data stack.
   **/
  def evalConditionFn: (VM=>VM);

  /**
   * eval condition in vm environment to true or false.
   **/
  def evalCondition(vm:VM):Boolean;


}
