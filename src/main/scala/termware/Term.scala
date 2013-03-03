package termware

import scala.math._
import scala.util._
import scala.reflect.runtime.universe._

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

  def isX : Boolean

  /**
   * unify self with term <code> y </code>
   */
  def unify(y:Term, s: Substitution): UnificationResult

  /**
   * generic substitution, where in right part can be any term
   */
  def substg(s:Substitution): Term

  /**
   * substitution where in right part can be only variable definitions.
   */
  def substv(s:Substitution): Term

}


/**
 * typeclass for term algebra.
 */
abstract class ToTerm[X:TypeTag]
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

   // signature part

   def  xunify(x: X, y: Term, s: Substitution): UnificationResult

   def  xsubstg(x: X, s: Substitution): Term

   def  xsubstv(x: X, s: Substitution): Term

}

trait BaseAsTerm extends Term
{
  type ValueType;
  type WrapperType <: ToTerm[ValueType];
}

case class AsTerm[V:TypeTag](v:V, tt: ToTerm[V]) extends BaseAsTerm 
{

   type ValueType = V;

   def name:Name = tt.name(v)
         
   def arity:Int = tt.arity(v)

   def subterms: IndexedSeq[Term] = 
                            tt.subterms(v)

   def nameSubterms:Map[Name,Term] = 
                            tt.nameSubterms(v)

   def isAtom: Boolean = tt.isAtom(v)

   def isPrimitive: Boolean = tt.isPrimitive(v)

   def isNumber: Boolean = tt.isNumber(v)

   def is[T](implicit ttag: TypeTag[T]): Boolean = tt.is[T](v)

   def value[T](implicit ttag: TypeTag[T]): Option[T] = tt.value[T](v)

   def numberValue: Option[ScalaNumericAnyConversions] = tt.numberValue(v)

   def original: V = v

   def isX: Boolean = false

   def  unify(y: Term, s: Substitution): UnificationResult 
     = tt.xunify(v,y,s)

   def substg(s:Substitution): Term
     = tt.xsubstg(v, s)

   def substv(s:Substitution): Term
     = tt.xsubstv(v, s)

}

object AsTerm
{
    // can't use apply, because signature will be the same as in default case-class apply
   def create[X](x:X)(implicit tt:ToTerm[X], ttag:TypeTag[X]) = new AsTerm(x, tt)
}


// vim: set ts=4 sw=4 et:
