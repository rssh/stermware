package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

trait SimpleSubst extends Term
{

  override def subst(s: PartialFunction[Term,Term])
                    (implicit ctx:CallContext):ComputationBounds[Term]
    =  Done(fixSubst(s));

}
