package ua.gradsoft.termware;

import ua.gradsoft.termware.parser.OperatorSyntax;

trait Theory extends TermWareInstance
{

 def booleanSignature: BooleanTermSignature;

 def stringSignature: StringTermSignature;

 def charSignature: TermSignature;

 def byteSignature: TermSignature;

 def shortSignature: TermSignature;

 def intSignature: IntTermSignature;

 def longSignature: TermSignature;

 def doubleSignature: TermSignature;

 def floatSignature: TermSignature;

 def bigIntSignature: TermSignature;

 def bigDecimalSignature: TermSignature;

 def atomSignature(name:Name): TermSignature;

 def freeAtomSignature: TermSignature;

 def nilSignature: TermSignature;

 def freeFunSignature: TermSignature;

 def typeAlgebra: TypeAlgebra;

 def atomSignature(name:String): TermSignature =
       atomSignature(symbolTable.getOrCreate(name));

 def funSignature(name:Name): TermSignature;
 def funSignature(name:String): TermSignature =
       funSignature(symbolTable.getOrCreate(name));

 def arraySignature: TermSignature;
 def listSignature: TermSignature;

 def etaSignature: EtaTermSignature; 
 def xSignature: XTermSignature; 

 def refSignature: TermSignature;

 def withSignature: WithTermSignature;

 def errorSignature: ErrorTermSignature;

 def operatorSyntax: OperatorSyntax;

 def createFunTerm(name:Name, args:Term*):Term
              = funSignature(name).createTerm(name,args:_*);

 def createFunTerm(name:String, args:Term*):Term 
              = createFunTerm(symbolTable.getOrCreate(name),args:_*);

 def toAnyRef(t:Term):AnyRef = t.signature.toAnyRef(t);

 def toAny(t:Term): Any = t.signature.toAny(t);

 def fromAnyRef(x:AnyRef): Term;

 def fromAny(x:Any): Term;

}
