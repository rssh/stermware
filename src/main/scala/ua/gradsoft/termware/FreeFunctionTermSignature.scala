package ua.gradsoft.termware;

class FreeFunctionalTermSignature(th:Theory) 
                                    extends FunctionalTermSignature
{

 def fixedArity:Option[Int] = None;

 def indexByName:Option[Name=>Option[Int]] = None;
 def nameByIndex:Option[Int=>Option[Name]] = None;

 def fixedName:Option[Name] = None;
 
 def createTerm(name:Name, args:RandomAccessSeq[Term]) : Option[Term] =  
         Some(new FunctionalTerm(name,args,this));
 
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
                ).get;
   val typeOut = theory.typeAlgebra.reduce(typeIn);
   return typeOut;
 }
 

 val theory = th;
}
