package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

trait SimpleCompare extends Term
{

  def termCompare(t:Term)(implicit ctx:CallContext):ComputationBounds[Int]
                                                       = Done(fixTermCompare(t));

}
