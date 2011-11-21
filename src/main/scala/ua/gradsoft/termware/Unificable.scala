package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

trait Unificable[T] extends Ordered[T]
{


  def unify(t:T, s: Substitution[T])(implicit ctx:CallContext):
                      ComputationBounds[(Boolean,Substitution[T])];

  def onUnify[A](t:T, s:Substitution[T])
         (cont:((Boolean,Substitution[T]),CallContext)=>ComputationBounds[A])
         (implicit ctx:CallContext, mt:Manifest[T], ma:Manifest[A]) =
  {
     val f = Call{ (ctx:CallContext) => unify(t,s)(ctx); };
     CallCC.compose(f,cont);
  };

}


