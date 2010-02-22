package ua.gradsoft.termware;

trait Theory extends TermWareInstance
{

 def booleanSignature: BooleanTermSignature;

 def atomSignature(name:Name): TermSignature;

 def freeAtomSignature: TermSignature;

 def nilSignature: TermSignature;

 def freeFunSignature: TermSignature;


 def typeAlgebra: TypeAlgebra;


 def atomSignature(name:String): TermSignature =
       atomSignature(symbolTable.getOrCreateElement(name));

 def funSignature(name:Name, args:RandomAccessSeq[Term]): TermSignature;
 def funSignature(name:String, args:RandomAccessSeq[Term]): TermSignature =
       funSignature(symbolTable.getOrCreateElement(name), args);

 def etaSignature: TermSignature; 
 def etaXSignature: EtaXTermSignature; 

 def errorSignature: ErrorTermSignature;

}
