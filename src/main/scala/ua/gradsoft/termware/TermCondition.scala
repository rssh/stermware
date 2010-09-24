package ua.gradsoft.termware;

trait TermCondition 
{

  /**
   * can we quickly say, that this condition is evaluated to false
   * without futer evaluation
   **/
  def isQuickFalse: Boolean


  /**
   * can we quickly say, that this condition is evaluated to true
   * without future evaluation
   **/
  def isQuickTrue:  Boolean;
  
  /**
   * return function which eval condition and leave result on data stack.
   **/
  def evalConditionFn(s:Substitution): (VM=>VM);

  /**
   * eval condition in vm environment to true or false.
   **/
  def evalCondition(s:Substitution)(vm:VM):Boolean;


}
