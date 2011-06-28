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

object TrueTermCondition extends TermCondition
{

  def isQuickTrue=true;
  def isQuickFalse=false;
  def evalConditionFn(s:Substitution) = Done(true);

}

object FalseTermCondition extends TermCondition
{

  def isQuickTrue=false;
  def isQuickFalse=true;
  def evalConditionFn(s:Substitution) = Done(false);

}

object TermCondition
{

   def apply(v:Boolean):TermCondition =
    v match {
       case true => TrueTermCondition
       case false => FalseTermCondition
    }

   def apply(t:Term):TermCondition = build(t);

   def build(t:Term):TermCondition =
   {
     if (t.isBoolean) {
         apply(t.getBoolean);
     } else {
         //TODO: implement
         throw new UnsupportedOperationException("not implemented");
     }
   }

}
