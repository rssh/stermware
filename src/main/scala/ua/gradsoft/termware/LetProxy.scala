package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import java.io.PrintWriter;
import ua.gradsoft.termware.flow._;

/**
 * class represent variable, bound in let-term.
 *(All methods are delegated to proxy)
 **/
case class LetProxy(val letName: Name, 
               val letLabel:Int, 
               val letOwner :LetTerm) extends Term
                      with ComplexCompare
                      with ComplexUnify
                      with ComplexSubst
{


  override def isBoolean: Boolean =
    (if (isComputed) proxy.isBoolean else throwUOE);

  override def getBoolean: Boolean =
    (if (isComputed) proxy.getBoolean else throwUOE);

  def isByte:  Boolean =
    (if (isComputed) proxy.isByte else throwUOE);

  def getByte: Byte =
    (if (isComputed) proxy.getByte else throwUOE);

  def isShort:  Boolean =
    (if (isComputed) proxy.isShort else throwUOE);

  def getShort: Short =
    (if (isComputed) proxy.getShort else throwUOE);

  def isInt:  Boolean =
    (if (isComputed) proxy.isInt else throwUOE);

  def getInt: Int =
    (if (isComputed) proxy.getInt else throwUOE);

  def isLong:  Boolean =
    (if (isComputed) proxy.isLong else throwUOE);

  def getLong: Long =
    (if (isComputed) proxy.getLong else throwUOE);

  def isBigInt: Boolean =
    (if (isComputed) proxy.isBigInt else throwUOE);

  def getBigInt: BigInt =
    (if (isComputed) proxy.getBigInt else throwUOE);

  def isBigDecimal: Boolean =
    (if (isComputed) proxy.isBigDecimal else throwUOE);

  def getBigDecimal: BigDecimal =
    (if (isComputed) proxy.getBigDecimal else throwUOE);

  def isFloat: Boolean =
    (if (isComputed) proxy.isFloat else throwUOE);

  def getFloat: Float =
    (if (isComputed) proxy.getFloat else throwUOE);

  def isDouble: Boolean =
    (if (isComputed) proxy.isDouble else throwUOE);

  def getDouble: Double =
    (if (isComputed) proxy.getDouble else throwUOE);

  def isNumber: Boolean =
    (if (isComputed) proxy.isNumber else throwUOE);

  def getNumber: Number =
    (if (isComputed) proxy.getNumber else throwUOE);

  def getNumberKind: Int =
    (if (isComputed) proxy.getNumberKind else throwUOE);

  override def isChar:  Boolean =
    (if (isComputed) proxy.isChar else throwUOE);

  override def getChar: Char  =
    (if (isComputed) proxy.getChar else throwUOE);

  override def isString:  Boolean = 
    (if (isComputed) proxy.isString else throwUOE);

  override def getString: String = 
    (if (isComputed) proxy.getString else throwUOE);

  override def isException: Boolean = 
    (if (isComputed) proxy.isException else throwUOE);

  override def getException: Exception = 
    (if (isComputed) proxy.getException else throwUOE);

  override def getMessage: String =
    (if (isComputed) proxy.getMessage else throwUOE);

  override def isRef: Boolean = 
    (if (isComputed) proxy.isRef else throwUOE);

  override def getRef: AnyRef =
    (if (isComputed) proxy.getRef else throwUOE);


   def arity:Int = 
    (if (isComputed) proxy.arity else 0);

   def name =
    (if (isComputed) proxy.name else letName);

   def subterm(i:Int) = 
    (if (isComputed) proxy.subterm(i) else throwUOE);

   def subterms = 
    (if (isComputed) proxy.subterms else throwUOE);

   override def xOwner:XOwner = 
    (if (isComputed) proxy.xOwner else throwUOE);

   def unify(t:Term, s:Substitution)(implicit ctx:CallContext) = {
      CallCC.compose(computed,
              { (x:Term,ctx:CallContext) => x.unify(t,s)(ctx) }
             );
   }

   override def isError: Boolean = 
     (if (isComputed) proxy.isError else throwUOE);

   override def isEta: Boolean = 
     (if (isComputed) proxy.isEta else throwUOE);

   override def isAtom: Boolean = 
     (if (isComputed) proxy.isEta else throwUOE);

   override def isNil: Boolean = 
     (if (isComputed) proxy.isEta else throwUOE);

   override def termClassIndex: Int = TermClassIndex.LET_PROXY

   override def termHashCode: Int =
     (if (isComputed) proxy.termHashCode else letLabel+letOwner.termHashCode);

   override def termCompare(t:Term)(implicit ctx:CallContext)
                                                :ComputationBounds[Int] =
      CallCC.compose(computed,
          { (x:Term,ctx:CallContext) => x.termCompare(t)(ctx) }
      );


   override def onTermCompare[T](t:Term)
                      (cont:(Int,CallContext) => ComputationBounds[T])
                      (implicit ctx:CallContext) : ComputationBounds[T] =
      CallCC.compose(computed,
              { (x:Term,ctx:CallContext) => x.onTermCompare(t)(cont)(ctx) }
      );


   override def termEq(t:Term)(implicit ctx:CallContext): ComputationBounds[Boolean] = 
      CallCC.compose(computed,
             { (x:Term,ctx:CallContext) => x.termEq(t)(ctx) }
      );

   def subst(s: PartialFunction[Term,Term])(implicit ctx:CallContext):
                                                     ComputationBounds[Term]
    =
      CallCC.compose(computed,
             { (x:Term,ctx:CallContext) => x.subst(s)(ctx) }
      );


   override def print(out:PrintWriter):Unit = 
      out.print(letName);

   def signature: TermSignature =
     (if (isComputed) proxy.signature else throwUOE);

   def attributes =
    (if (isComputed) proxy.attributes else throwUOE);


   def proxy: Term = computed.result.get;

   def isComputed: Boolean = computed.isDone;

   def computed: ComputationBounds[Term] =
                       letOwner.vars(letLabel).value;


}

