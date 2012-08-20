import language.implicitConversions

import scala.reflect.runtime.universe._


package object termware {


 implicit def toTerm[T:TypeTag](x:T)(implicit ops:ToTerm[T]) = new AsTerm(x,ops)

 implicit val intToTerm = IntToTerm  
 implicit val longToTerm = LongToTerm
 implicit val floatToTerm = FloatToTerm
 implicit val doubleToTerm = DoubleToTerm
 
 implicit val bigDecimalToTerm = new NumericToTerm[BigDecimal]
 implicit val bigIntToTerm = new NumericToTerm[BigInt]
  
 implicit val stringToTerm = new NonNumericPrimitiveToTerm[String]

}
