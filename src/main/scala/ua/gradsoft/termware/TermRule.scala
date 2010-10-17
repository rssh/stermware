package ua.gradsoft.termware;


case class TermRuleBranch(val condition:TermCondition, val result:Term)

case class TermRule(val pattern:Term, branches: List[TermRuleBranch])
{
  def withCondition:Boolean = 
         branches.find(!_.condition.isQuickTrue)!=None;

}

object TermRule {

  def build(t:Term):TermRule = {
    throw new Exception("Not implemented"); 
  }

  def apply(t:Term) = build(t);

}
