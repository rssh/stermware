package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

trait SimpleUnify extends Term
{

  override def unify(t:Term, s: Substitution)
                    (implicit ctx:CallContext)
                               :ComputationBounds[(Boolean,Substitution)]
    =  Done(fixUnify(t,s))

}
