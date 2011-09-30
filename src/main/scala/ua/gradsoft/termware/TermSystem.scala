package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

trait TermSystem
{

  def theory: Theory;

  def reduce(t: Term): Pair[Term, Boolean]

  def facts: FactsDatabase;

  def reduce(ct: ComputationBounds[Term])(implicit ctx:CallContext): ComputationBounds[Pair[ComputationBounds[Term], Boolean]]

}
