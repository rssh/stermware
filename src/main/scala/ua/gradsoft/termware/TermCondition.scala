package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

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
  def evalConditionFn(s:Substitution): ComputationBounds[Boolean]


}
