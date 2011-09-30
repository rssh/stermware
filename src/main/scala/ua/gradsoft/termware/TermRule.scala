package ua.gradsoft.termware;


case class TermRuleBranch(val condition:TermCondition, val result:Term)

case class TermRule(val pattern:Term, branches: List[TermRuleBranch])
{
  def withCondition:Boolean = 
         branches.find(!_.condition.isQuickTrue)!=None;

}

class TermRuleBuilder(ts:TermSystem) extends DefaultTermNames
{

  def theory = ts.theory;

  def build(t:Term):Seq[TermRule] = {
    t match {
       case Term(Rule, Seq(pattern,destination), _) =>
                val condition=TermCondition(true);
                TermRule(pattern, List(TermRuleBranch(condition, destination)))::Nil
       case Term(ConditionalRule, Seq(pattern, destinations), _ ) =>
                var rbranches = List[TermRuleBranch]();
                var cb=destinations;
                while(!cb.isNil) {
                  val b=cb.subterm(0);
                  rbranches=TermRuleBranch(
                             TermCondition(b.subterm(0),ts),b.subterm(1))::rbranches;
                  cb=cb.subterm(1);
                }
                TermRule(pattern, rbranches.reverse)::Nil
       //case Term(Eta,Seq(left, right), _) =>
       //case Term(Eta,Seq(left, right,cont), _) => TermRule(left,right)::build(cont)
       //case Term(Let,Seq(assignments,term),_) => build(term,TermBinding(assigments))::Nil
       case _ =>
                throw new IllegalArgumentException("Invalid term for building rule:"+t.toString);
    }
  }

}


object TermRule
{
  def build(t:Term, ts:TermSystem) = new TermRuleBuilder(ts).build(t);
}


