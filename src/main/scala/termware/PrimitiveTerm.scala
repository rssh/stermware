package termware

import scala.reflect.runtime.universe._
import scala.math._


abstract class PrimitiveToTerm[X:TypeTag] extends ToTerm[X]
{

   def name(x:X):Name = PrimitiveName[X](x)

   def arity(x:X) = 0

   def subterms(x:X):IndexedSeq[BaseAsTerm] = IndexedSeq()

   def nameSubterms(x:X):Map[Name,BaseAsTerm] = Map()

   def isAtom(x:X) = false

   def isPrimitive(x:X) = true

   def is[T](x:X)(implicit ttag:TypeTag[T]):Boolean =
                               (typeOf[T] <:< typeOf[X]) 
     
   def value[T](t:X)(implicit ttag:TypeTag[T]):Option[T] =
      if (typeOf[T] <:< typeOf[X]) {
         Some(t.asInstanceOf[T])
      } else {
         None
      }

   def v(x:X):X = x


}

class NumericToTerm[X <% ScalaNumericConversions](implicit xtag:TypeTag[X]) extends PrimitiveToTerm[X]
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

class NonNumericPrimitiveToTerm[X:TypeTag] extends PrimitiveToTerm[X]
{

   def isNumber(x:X) = false

   def numberValue(x:X) = None

}

//
//object StringToTerm extends NonNumericPrimitiveToTerm[String]


// vim: set ts=4 sw=4 et:
