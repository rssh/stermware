package ua.gradsoft.termware;


case class TermRuleBranch(val condition:TermCondition, val result:Term)

case class TermRule(val pattern:Term, branches: List[TermRuleBranch])

