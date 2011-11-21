package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

/**
 * standart implementation of term system,
 **/
class StandardTermSystem(
                         override val theory: Theory,
                         val matching: MatchingNet,
                         val facts: FactsDatabase,
                         val strategy: Strategy
                        ) extends TermSystem
{

   def reduce(t:Term) : Pair[Term,Boolean] =
   {
    //val (cbt:ComputationBounds[Term],r:Boolean) = reduceStep(t);
    val (cbt,r) = reduceStep(t);
    return (CallCC.trampoline(cbt),r);
   }

   def reduceStep(t:Term):Pair[ComputationBounds[Term],Boolean] =
    CallCC.trampoline(Call{ (ctx:CallContext) => strategy(t,matching)(ctx) });
   
   def reduce(ct:ComputationBounds[Term])(implicit ctx:CallContext) : ComputationBounds[Pair[ComputationBounds[Term],Boolean]] =
    CallCC.compose(ct, (t:Term, ctx:CallContext) => strategy(t,matching)(ctx) );

}
