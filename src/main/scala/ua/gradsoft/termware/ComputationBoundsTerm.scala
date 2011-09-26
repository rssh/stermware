package ua.gradsoft.termware;

import java.io.PrintWriter;
import ua.gradsoft.termware.flow._;

class ComputationBoundsTerm(val cbt: ComputationBounds[Term]) extends Term
{

   private var isComputed = false;
   lazy val term = { val x=CallCC.trampoline(cbt); isComputed=true; x; }

   def isBoolean: Boolean = term.isBoolean;

   def getBoolean: Boolean = term.getBoolean;

   def isByte:  Boolean = term.isByte;
  
   def getByte: Byte = term.getByte;

   def isShort:  Boolean = term.isShort;

   def getShort: Short = term.getShort;

   def isInt:  Boolean = term.isInt;

   def getInt: Int = term.getInt;

   def isLong:  Boolean = term.isLong;

   def getLong: Long = term.getLong;

   def isBigInt: Boolean = term.isBigInt;

   def getBigInt: BigInt = term.getBigInt;

   def isBigDecimal: Boolean = term.isBigDecimal;

   def getBigDecimal: BigDecimal = term.getBigDecimal;

   def isFloat: Boolean = term.isFloat;

   def getFloat: Float = term.getFloat;

   def isDouble: Boolean = term.isDouble;

   def getDouble: Double = term.getDouble;

   def isNumber: Boolean  = term.isNumber;

   def getNumber: Number = term.getNumber;

   def getNumberKind: Int = term.getNumberKind;

   override def isChar:  Boolean = term.isChar;

   override def getChar: Char = term.getChar;

   override def isString:  Boolean = term.isString;

   override def getString: String = term.getString;

   override def isException: Boolean = term.isException;

   override def getException: Exception = term.getException;

   override def getMessage: String = term.getMessage;

   override def isRef: Boolean = term.isRef;

   override def getRef: AnyRef = term.getRef;

   def arity: Int = term.arity;

   override def patternArity: Option[Int] = term.patternArity;

   def subterm(i:Int): Term = term.subterm(i);

   def subterms: IndexedSeq[Term] = term.subterms;

   def name: Name = term.name;

   override  def patternName: Option[Name] = term.patternName;

   def signature: TermSignature = term.signature;

   override def theory: Theory = term.theory;

   override def termType: Term = term.termType;

   override def isX: Boolean = term.isX;

   override def xOwner: XOwner = term.xOwner;

   override def xLabel: Int = term.xLabel;

   def isNil: Boolean = term.isNil;

   def isAtom: Boolean = term.isAtom;

   def isEta: Boolean = term.isEta;

   def isError: Boolean = term.isError;

   def subst(s: PartialFunction[Term,Term])(implicit ctx:CallContext):
                                                     ComputationBounds[Term] =
        term.subst(s)(ctx);  // TODO: rethink.
     
   def fixSubst(s: PartialFunction[Term,Term]): Term = term.fixSubst(s);

   def unify(t:Term, s: Substitution[Term])(implicit ctx:CallContext):
                              ComputationBounds[(Boolean,Substitution[Term])] = term.unify(t,s);

   def fixUnify(t:Term, s:Substitution[Term]): (Boolean, Substitution[Term]) = term.fixUnify(t,s);


   def termClassIndex: Int = term.termClassIndex;

   def fixTermCompare(t:Term): Int = term.fixTermCompare(t);

   def termCompare(t:Term)(implicit ctx:CallContext):ComputationBounds[Int] 
        = term.termCompare(t)(ctx);

   def fixTermEq(t:Term): Boolean = term.fixTermEq(t);

   def termHashCode: Int = term.termHashCode;

   def print(out: PrintWriter): Unit = term.print(out);

   def attributes = term.attributes;

   override def toComputationBounds: ComputationBounds[Term] =
   {
     if (isComputed) Done(term) else cbt;
   }


}

object ComputationBoundsTerm
{

   def apply(ct:ComputationBounds[Term]) : Term = 
     if (ct.isDone) ct.result.get else new ComputationBoundsTerm(ct);

}
