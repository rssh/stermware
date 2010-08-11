package ua.gradsoft.termware.freeterms;

import ua.gradsoft.termware._;

class FreeTermWithFixedNameAnyAritySignature(n:Name,
                                             th:Theory)
                                        extends FunctionalTermSignature
                                              with FixedNameSignature
                                              with AnyAritySignature
{

 def createTerm(name:Name, args:IndexedSeq[Term]) : Term =
              new FreeTermWithFixedNameAnyArity(args,this);

 def getType(t:Term):Term = {
   t.getAttribute(theory.symbolTable.TYPE) match {
      case Some(x) =>  x
      case None   => {
        val r = calculateType(t);
        t.setAttribute(theory.symbolTable.TYPE, r);
        r
      }
   }
 }

 def calculateType(t:Term):Term = {
   val typeIn = theory.freeFunSignature.createTerm(
                   t.name,
                   t.subterms.map( x => x.signature.getType(x) )
                );
   val typeOut = theory.typeAlgebra.reduce(typeIn);
   return typeOut;
 }

 val name=n;
 val theory=th;

}
