package termware

import scala.reflect.runtime.universe._
import scala.math._
import scala.util.{Try => UTry}


abstract class PrimitiveToTerm[X](implicit xtag: TypeTag[X], ord:Ordering[X]) extends ToTerm[X]
{

   def name(x:X):Name = PrimitiveName[X](x)

   def arity(x:X) = 0

   def subterms(x:X):IndexedSeq[BaseAsTerm] = IndexedSeq()

   def nameSubterms(x:X):Map[Name,BaseAsTerm] = Map()

   @inline def isAtom(x:X) = false

   @inline def isPrimitive(x:X) = true
   
   @inline def isComplex(x:X) = false

   def is[T](x:X)(implicit ttag:TypeTag[T]):Boolean =
                               (typeOf[T] <:< typeOf[X]) 
     
   def value[T](t:X)(implicit ttag:TypeTag[T]):Option[T] =
      if (typeOf[T] <:< typeOf[X]) {
         Some(t.asInstanceOf[T])
      } else {
         None
      }

   type VType = X

   def vTag: TypeTag[X] = xtag

   def v(x:X):X = x

   def  xunify(x: X, y: Term, s: Substitution): UnificationResult = ???

   def  xsubstg(x: X, s: Substitution): Term =
   {
     val t = AsTerm[X](x,this)
     s.getOrElse(t,t)
   }

   def  xsubstv(x: X, s: Substitution): Term
        = AsTerm[X](x, this)

}

class NumericToTerm[X <% ScalaNumericAnyConversions](implicit xtag:TypeTag[X], ord:Ordering[X]) extends PrimitiveToTerm[X]
{

   override def isNumber(x:X) = true

   override def numberValue(x:X) = Some(x)

}

object IntToTerm extends NumericToTerm[Int]

object LongToTerm extends NumericToTerm[Long]

object FloatToTerm extends NumericToTerm[Float]

object DoubleToTerm extends NumericToTerm[Double]

// bag in compiler ???
//  (now implicit val is instantiated in package object)
//object BigDecimalToTerm extends NumericToTerm[BigDecimal]
// 
//object BigIntToTerm extends NumericToTerm[BigInt](typeTag[BigInt])

class NonNumericPrimitiveToTerm[X:TypeTag](implicit ord:Ordering[X]) extends PrimitiveToTerm[X]
{

   def isNumber(x:X) = false

   def numberValue(x:X) = None

}

//
//object StringToTerm extends NonNumericPrimitiveToTerm[String]


// vim: set ts=4 sw=4 et:
