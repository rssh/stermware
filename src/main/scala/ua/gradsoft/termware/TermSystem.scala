package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

trait TermSystem
{

  def theory: Theory;

  def reduce(ct:ComputationBounds[Term])(implicit ctx:CallContext) 
                                                  :ComputationBounds[Term];

}
