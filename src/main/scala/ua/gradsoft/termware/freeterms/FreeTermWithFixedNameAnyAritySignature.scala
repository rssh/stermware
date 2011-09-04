package ua.gradsoft.termware.freeterms;

import ua.gradsoft.termware._;
import ua.gradsoft.termware.flow._;

class FreeTermWithFixedNameAnyAritySignature(n:Name,
                                             th:Theory)
                                        extends FunctionalTermSignature
                                              with FixedNameSignature
                                              with AnyAritySignature
{

 def createTerm(name:Name, args:IndexedSeq[Term]) : Term =
              new FreeTermWithFixedNameAnyArity(args,this);

 def termType(ct:ComputationBounds[Term])(implicit ctx:CallContext):
                                           ComputationBounds[Term] = {
  if (ct.isDone) {
   val t = ct.result.get;
   t.getAttribute(theory.symbolTable.TYPE) match {
      case Some(x) =>  Done(x)
      case None   => {
        val typeIn = theory.freeFunSignature.createTerm(
                        t.name,
                        t.subterms.map( _.termType )
                     );
        val typeOut = theory.typeAlgebra.reduce(typeIn);
        t.setAttribute(theory.symbolTable.TYPE, new ComputationBoundsTerm(typeOut));
        typeOut
      }
   }
  } else {
   CallCC.compose(ct, { (t:Term, ctx:CallContext) => termType(Done(t))(ctx) });
  }
 }

 val name=n;
 val theory=th;

}
