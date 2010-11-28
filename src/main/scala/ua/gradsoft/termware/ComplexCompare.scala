package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

trait ComplexCompare extends Term
{

  def fixTermCompare(t:Term):Int = 
         CallCC.trampoline(Call{ (ctx:CallContext) => termCompare(t)(ctx) });

  def fixTermEq(t:Term):Boolean = { fixTermCompare(t)==0; }

}
