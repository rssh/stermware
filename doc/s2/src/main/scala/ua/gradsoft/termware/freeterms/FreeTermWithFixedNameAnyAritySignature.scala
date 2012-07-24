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

 def termType(t:Term):Term = {
   t.getAttribute(theory.symbolTable.TYPE) match {
      case Some(x) => x
      case None   => {
        val typeIn = theory.freeFunSignature.createTerm(
                        t.name,
                        t.subterms.map( _.termType )
                     );
        val typeOut = theory.typeAlgebra.reduce(typeIn)._1;
        t.setAttribute(theory.symbolTable.TYPE, typeOut);
        typeOut
      }
   }
 }

 val name=n;
 val theory=th;

}
