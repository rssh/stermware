package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

/**
 * generic signature of term in free algebra.
 **/
class FreeFunctionalTermSignature(val theory:Theory) 
                                    extends FunctionalTermSignature
{

 def fixedArity:Option[Int] = None;

 def indexByName:Option[Name=>Option[Int]] = None;
 def nameByIndex:Option[Int=>Option[Name]] = None;

 def fixedName:Option[Name] = None;
 
 def createTerm(name:Name, args:IndexedSeq[Term]) : Term =  
         new FreeFunctionalTerm(name,args,this);
 
 def termType(t:Term):Term = {
   t.getAttribute(theory.symbolTable.TYPE) match {
      case Some(x) => x
      case None   => {
        val typeIn = theory.freeFunSignature.createTerm(
                        t.name,
                        t.subterms.map{ _.termType }
                     );
        val typeOut = theory.typeAlgebra.reduce(typeIn)._1;
        t.setAttribute(theory.symbolTable.TYPE, typeOut);
        typeOut;
      }
   }
 } 
 

}
