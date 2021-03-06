package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

trait ComplexUnify extends Term
{

  def fixUnify(t:Term, s:Substitution[Term]): (Boolean, Substitution[Term]) = 
         CallCC.trampoline(Call{ (ctx:CallContext) => unify(t,s)(ctx) });

}
