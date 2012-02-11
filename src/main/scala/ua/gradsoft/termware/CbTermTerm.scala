package ua.gradsoft.termware;

import java.io._;
import ua.gradsoft.termware.flow._;


class CbTermTerm(cb:CbTerm, implicit val ctx:CallContext) extends Term
                                                           with ComplexUnify
                                                           with ComplexSubst
                                                           with ComplexCompare
{
	
	
  def arity: Int = CallCC.trampoline(cb.arity);

  override def patternArity: Option[Int] = CallCC.trampoline(cb.patternArity);

  def subterm(i:Int): Term = cb.subterm(i)(ctx).toTerm(ctx);

  def subterms: IndexedSeq[Term] = 
                      CallCC.trampoline(cb.subterms).map(_.toTerm(ctx));

  def name: Name = CallCC.trampoline(cb.name);

  override def patternName: Option[Name] = CallCC.trampoline(cb.patternName);

  def signature: TermSignature = CallCC.trampoline(cb.signature);

  override def theory: Theory = CallCC.trampoline(cb.theory);

  override def isX: Boolean = CallCC.trampoline(cb.isX);

  override def xOwner: XOwner = CallCC.trampoline(cb.xOwner);

  override def xLabel: Int = CallCC.trampoline(cb.xLabel);

  def isNil: Boolean = CallCC.trampoline(cb.isNil);

  def isAtom: Boolean = CallCC.trampoline(cb.isAtom);

  def isEta: Boolean = CallCC.trampoline(cb.isEta);

  def isLet: Boolean = CallCC.trampoline(cb.isLet);

  def isWith: Boolean = CallCC.trampoline(cb.isWith);

  def isError: Boolean = CallCC.trampoline(cb.isError);

  def optValue[T](implicit mt:Manifest[T]) = CallCC.trampoline(cb.optValue[T](mt))
  
  override def isBoolean: Boolean = CallCC.trampoline(cb.isBoolean);

  override def getBoolean: Boolean = CallCC.trampoline(cb.getBoolean);

  def isByte:  Boolean = CallCC.trampoline(cb.isByte);

  def getByte: Byte = CallCC.trampoline(cb.getByte);

  def isShort:  Boolean = CallCC.trampoline(cb.isShort);

  def getShort: Short = CallCC.trampoline(cb.getShort);

  def isInt:  Boolean = CallCC.trampoline(cb.isInt);

  def getInt: Int = CallCC.trampoline(cb.getInt);

  def isLong:  Boolean = CallCC.trampoline(cb.isLong);

  def getLong: Long = CallCC.trampoline(cb.getLong);

  def isBigInt: Boolean = CallCC.trampoline(cb.isBigInt);

  def getBigInt: BigInt = CallCC.trampoline(cb.getBigInt);

  def isBigDecimal: Boolean = CallCC.trampoline(cb.isBigDecimal);

  def getBigDecimal: BigDecimal = CallCC.trampoline(cb.getBigDecimal);

  def isFloat: Boolean = CallCC.trampoline(cb.isFloat);

  def getFloat: Float = CallCC.trampoline(cb.getFloat);

  def isDouble: Boolean = CallCC.trampoline(cb.isDouble);

  def getDouble: Double = CallCC.trampoline(cb.getDouble);

  def isNumber: Boolean = CallCC.trampoline(cb.isNumber);

  def getNumber: Number = CallCC.trampoline(cb.getNumber);

  def getNumberKind: Int = CallCC.trampoline(cb.getNumberKind);

  override def isChar:  Boolean =  CallCC.trampoline(cb.isChar);

  override def getChar: Char = CallCC.trampoline(cb.getChar);

  override def isString:  Boolean = CallCC.trampoline(cb.isString);

  override def getString: String = CallCC.trampoline(cb.getString);

  override def isException: Boolean = CallCC.trampoline(cb.isException);

  override def getException: Exception = CallCC.trampoline(cb.getException);

  override def getMessage: String = CallCC.trampoline(cb.getMessage);

  override def isRef: Boolean = CallCC.trampoline(cb.isRef);

  override def getRef: AnyRef = CallCC.trampoline(cb.getRef);

  def subst(s: PartialFunction[Term,Term])(implicit ctx:CallContext):
                                           ComputationBounds[Term] =
   CallCC.compose( cb.subst(CbTermUtil.cb(s)),
                  { (x:CbTerm) => x.toTermComputationBounds; } );

  def unify(t:Term, s: Substitution[Term])(implicit ctx:CallContext):
                           ComputationBounds[(Boolean,Substitution[Term])] =
    CallCC.compose(
           cb.unify(t.toCbTerm, CbTermUtil.cb(s)),
           { (x:(Boolean, Substitution[CbTerm])) => Done((x._1,CbTermUtil.uncb(x._2))); }
    );
  

  def termClassIndex: Int = CallCC.trampoline(cb.termClassIndex);

  def termCompare(t:Term)(implicit ctx:CallContext):ComputationBounds[Int] =
                                                    cb.termCompare(t.toCbTerm);

  def print(out: PrintWriter): Unit  = cb.print(out);

  def termHashCode: Int = hashCode;

  def attributes: AttributeMapType = cb.attributes;

  override def toCbTerm: CbTerm = cb;

  

}
