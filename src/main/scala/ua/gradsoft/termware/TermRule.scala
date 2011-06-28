package ua.gradsoft.termware;


case class TermRuleBranch(val condition:TermCondition, val result:Term)

case class TermRule(val pattern:Term, branches: List[TermRuleBranch], vars:Map[Int,EtaXTerm]=Map())
{
  def withCondition:Boolean = 
         branches.find(!_.condition.isQuickTrue)!=None;

}

class TermRuleBuilder(override val theory:Theory) extends DefaultTermNames
{

  def build(t:Term):TermRule = {
    t.name match {
      case Rule => val pattern=t.subterm(0);
                       val destination=t.subterm(1);
                       val condition=TermCondition(true);
                       TermRule(pattern, List(TermRuleBranch(condition, destination))); 
      case ConditionalRule =>
                       val pattern=t.subterm(0);
                       var cb=t.subterm(1);
                       var rbranches = List[TermRuleBranch]();
                       while(!cb.isNil) {
                         val b=cb.subterm(0);
                         rbranches=TermRuleBranch(
                                       TermCondition(b.subterm(0)),b.subterm(1))::rbranches;
                         cb=cb.subterm(1);
                       }
                       TermRule(pattern, rbranches.reverse); 
    }
  }

}


