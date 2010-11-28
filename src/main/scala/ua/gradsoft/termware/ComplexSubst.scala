package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

trait ComplexSubst extends Term
{

  override def fixSubst(s: PartialFunction[Term,Term]): Term =
              CallCC.trampoline(Call{(ctx:CallContext)=>subst(s)(ctx)});

}
