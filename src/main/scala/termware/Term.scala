package termware

import scala.reflect.runtime.universe._
import scala.math._

trait Term 
{

  def name: Name

  def arity: Int

  def subterms:IndexedSeq[Term]

  def subterm(i:Int): Option[Term] = 
       if (i<arity) Some(subterms(i)) else None 

  def isAtom:Boolean;

  def isPrimitive:Boolean;

  def isNumber: Boolean;

  def is[T](implicit ttag:TypeTag[T]):Boolean

  def value[T](implicit ttag:TypeTag[T]):Option[T]

  def numberValue: Option[ScalaNumericAnyConversions]

  def isX = false

  def signature: TermSignature

}


/**
 * typeclass for term algebra.
 */
abstract class ToTerm[X:TypeTag]
                    extends TermSignature
{

   type Origin = X

   def name(x:X):Name

   def arity(x:X):Int

   def subterms(x:X):IndexedSeq[BaseAsTerm]

   def subterm(x:X,i:Int): Option[BaseAsTerm] = 
          if (i<arity(x)) 
               Some(subterms(x)(i))
          else
               None
   
   def nameSubterms(x:X):Map[Name,BaseAsTerm]

   def nameSubterm(x:X,n:Name): Option[BaseAsTerm] =
               nameSubterms(x).get(n)

   def isAtom(x:X):Boolean;

   def isPrimitive(x:X):Boolean;

   def isNumber(x:X):Boolean;

   def is[T](x:X)(implicit ttag:TypeTag[T]):Boolean

   def value[T](x:X)(implicit ttag:TypeTag[T]):Option[T]

   def numberValue(x:X): Option[ScalaNumericAnyConversions]

   def isX(x:X):Boolean = false

   def xNum(x:X):Long = 0L

   def toTerm(x:X):Term = 
                new AsTerm(x,this)

}

trait BaseAsTerm extends Term
{
  type ValueType;
  type WrapperType <: ToTerm[ValueType];
}

class AsTerm[X:TypeTag](x:X, tt: ToTerm[X]) extends BaseAsTerm 
{

   type ValueType = X;

   def name:Name = tt.name(x)
         
   def arity:Int = tt.arity(x)

   def subterms: IndexedSeq[Term] = 
                            tt.subterms(x)

   def nameSubterms:Map[Name,Term] = 
                            tt.nameSubterms(x)

   def isAtom: Boolean = tt.isAtom(x)

   def isPrimitive: Boolean = tt.isPrimitive(x)

   def isNumber: Boolean = tt.isNumber(x)

   def is[T](implicit ttag: TypeTag[T]): Boolean = tt.is[T](x)

   def value[T](implicit ttag: TypeTag[T]): Option[T] = tt.value[T](x)

   def numberValue: Option[ScalaNumericAnyConversions] = tt.numberValue(x)

   def signature = tt

}




// vim: set ts=4 sw=4 et:
