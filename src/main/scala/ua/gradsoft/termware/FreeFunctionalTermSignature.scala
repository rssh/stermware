package ua.gradsoft.termware;

/**
 * generic signature of term in free algebra.
 **/
class FreeFunctionalTermSignature(th:Theory) 
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
                   t.subterms.map{ _.termType }
                );
   val typeOut = theory.typeAlgebra.reduce(typeIn);
   return typeOut;
 }

 val theory = th;
}
