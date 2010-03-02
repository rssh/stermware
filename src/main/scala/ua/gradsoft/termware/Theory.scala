package ua.gradsoft.termware;

trait Theory extends TermWareInstance
{

 def booleanSignature: BooleanTermSignature;

 def stringSignature: StringTermSignature;

 def charSignature: TermSignature;

 def byteSignature: TermSignature;

 def shortSignature: TermSignature;

 def intSignature: TermSignature;

 def longSignature: TermSignature;

 def doubleSignature: TermSignature;

 def bigIntSignature: TermSignature;

 def bigDecimalSignature: TermSignature;

 def atomSignature(name:Name): TermSignature;

 def freeAtomSignature: TermSignature;

 def nilSignature: TermSignature;

 def freeFunSignature: TermSignature;

 def typeAlgebra: TypeAlgebra;

 def atomSignature(name:String): TermSignature =
       atomSignature(symbolTable.getOrCreate(name));

 def funSignature(name:Name, args:RandomAccessSeq[Term]): TermSignature;
 def funSignature(name:String, args:RandomAccessSeq[Term]): TermSignature =
       funSignature(symbolTable.getOrCreate(name), args);

 def arraySignature: TermSignature;
 def listSignature: TermSignature;

 def etaSignature: TermSignature; 
 def etaXSignature: EtaXTermSignature; 

 def refSignature: TermSignature;

 def errorSignature: ErrorTermSignature;

}
