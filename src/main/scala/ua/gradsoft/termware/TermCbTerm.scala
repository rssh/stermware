package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

import java.io.PrintWriter;
import java.io.OutputStream;
import java.io.StringWriter;

/**
 * CbTerm wich holds term.
 **/
class TermCbTerm(val term:Term) extends CbTerm 
{

  def isBoolean: ComputationBounds[Boolean] = Done(term.isBoolean);

  def getBoolean: ComputationBounds[Boolean] = Done(term.getBoolean);

  def isByte:  ComputationBounds[Boolean] = Done(term.isByte);

  def getByte: ComputationBounds[Byte] = Done(term.getByte);

  def isShort:  ComputationBounds[Boolean] = Done(term.isShort);

  def getShort: ComputationBounds[Short] = Done(term.getShort);

  def isInt: ComputationBounds[Boolean] = Done(term.isInt);

  def getInt: ComputationBounds[Int] = Done(term.getInt);

  def isLong:  ComputationBounds[Boolean] = Done(term.isLong);

  def getLong: ComputationBounds[Long] = Done(term.getLong);

  def isBigInt: ComputationBounds[Boolean] = Done(term.isBigInt);

  def getBigInt: ComputationBounds[BigInt] = Done(term.getBigInt);

  def isBigDecimal: ComputationBounds[Boolean] = Done(term.isBigDecimal);

  def getBigDecimal: ComputationBounds[BigDecimal] = Done(term.getBigDecimal);

  def isFloat: ComputationBounds[Boolean] = Done(term.isFloat);

  def getFloat: ComputationBounds[Float] = Done(term.getFloat);

  def isDouble: ComputationBounds[Boolean] = Done(term.isDouble);

  def getDouble: ComputationBounds[Double] = Done(term.getDouble);

  def isNumber: ComputationBounds[Boolean] = Done(term.isNumber);

  def getNumber: ComputationBounds[Number] = Done(term.getNumber);

  def getNumberKind: ComputationBounds[Int] = Done(term.getNumberKind);

  def isChar:  ComputationBounds[Boolean] = Done(term.isChar);

  def getChar: ComputationBounds[Char] = Done(term.getChar);

  def isString:  ComputationBounds[Boolean] = Done(term.isString);

  def getString: ComputationBounds[String] = Done(term.getString);

  def isException: ComputationBounds[Boolean] = Done(term.isException);

  def getException: ComputationBounds[Exception] = Done(term.getException);

  def getMessage: ComputationBounds[String] = Done(term.getMessage);

  def isRef: ComputationBounds[Boolean] = Done(term.isRef);

  def getRef: ComputationBounds[AnyRef] = Done(term.getRef);


  def arity(implicit ctx:CallContext): ComputationBounds[Int] = Done(term.arity);

  override def patternArity(implicit ctx: CallContext):ComputationBounds[Option[Int]] = Done(term.patternArity);

  def subterm(i:Int)(implicit ctx:CallContext): CbTerm = term.subterm(i).toCbTerm;

  def subterms(implicit ctx:CallContext): ComputationBounds[IndexedSeq[CbTerm]]=
    Done(term.subterms.map(_.toCbTerm))

  def name(implicit ctx:CallContext): ComputationBounds[Name] = Done(term.name);

  override def patternName(implicit ctx:CallContext): ComputationBounds[Option[Name]] = Done(term.patternName);
      
  def signature(implicit ctx:CallContext): ComputationBounds[TermSignature] = Done(term.signature);

  def theory(implicit ctx:CallContext): ComputationBounds[Theory] = Done(term.theory);

  def isX(implicit ctx:CallContext): ComputationBounds[Boolean] = Done(term.isX);

  def xOwner(implicit ctx:CallContext): ComputationBounds[XOwner] = Done(term.xOwner);

  def xLabel(implicit ctx:CallContext): ComputationBounds[Int] = Done(term.xLabel);

  def isNil(implicit ctx:CallContext): ComputationBounds[Boolean] = Done(term.isNil);

  def isAtom(implicit ctx:CallContext): ComputationBounds[Boolean] = Done(term.isAtom);

  def isEta(implicit ctx:CallContext): ComputationBounds[Boolean] = Done(term.isEta);

  def isError(implicit ctx:CallContext): ComputationBounds[Boolean] = Done(term.isError);

  def subst(s: PartialFunction[CbTerm,CbTerm])(implicit ctx:CallContext): 
                                               ComputationBounds[CbTerm] =
    CallCC.compose( term.subst(CbTermUtil.uncb(s)),
                    (x:Term) => Done(x.toCbTerm)
                  );

  def unify(t:CbTerm, s: Substitution[CbTerm])(implicit ctx:CallContext):
                                ComputationBounds[(Boolean,Substitution[CbTerm])] =
   CallCC.compose(
       term.unify(t.toTerm,CbTermUtil.uncb(s)),
       { (x:(Boolean,Substitution[Term]),ctx:CallContext) => Done((x._1,CbTermUtil.cb(x._2)(ctx))); }
   );

  def termClassIndex(implicit ctx:CallContext): ComputationBounds[Int] =
        Done(term.termClassIndex);

  def termCompare(t:CbTerm)(implicit ctx:CallContext):ComputationBounds[Int]=
       term.termCompare(t.toTerm);

  override def toTerm(implicit ctx:CallContext): Term = term;

  override def toTermComputationBounds: ComputationBounds[Term] = Done(term);

  def print(out:PrintWriter):Unit = term.print(out);

  def attributes = term.attributes;

  def compare(x:CbTerm):Int = 
  {
    // we need compare for ordering in TreeSet inside Substitution.
    //so, let's out comapre be hash-based and call real
    //possible resource-intensive term only when we can't deduce
    // based on hash.
    val c = hashCode-x.hashCode;
    if (c!=0) {
      c;
    } else {
      if (this eq x) {
        0
      } else {
        val ctx = new CallContext();
        term.compare(x.toTerm(ctx));
      }
    }
  }

  override def hashCode: Int = term.hashCode;

  override def equals(x:Any) = 
  {
   x match {
     case cb: CbTerm => (compare(cb)==0)
     case _  => false;
   }
  }


}



