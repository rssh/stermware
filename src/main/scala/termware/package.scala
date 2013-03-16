import language.implicitConversions

import scala.reflect.runtime.universe.TypeTag


package object termware {


 implicit def toTerm[T:TypeTag](x:T)(implicit ops:ToTerm[T]) = new AsTerm(x,ops)

 implicit val intToTerm = IntToTerm  
 implicit val longToTerm = LongToTerm
 implicit val floatToTerm = FloatToTerm
 implicit val doubleToTerm = DoubleToTerm
 
 implicit val bigDecimalToTerm = new NumericToTerm[BigDecimal]
 implicit val bigIntToTerm = new NumericToTerm[BigInt]
  
 implicit val stringToTerm = new NonNumericPrimitiveToTerm[String]
 implicit val charToTerm = new NonNumericPrimitiveToTerm[Char]
 implicit val booleanToTerm = new NonNumericPrimitiveToTerm[Boolean]
 
 
 implicit val atomTerm = AtomToTerm

 implicit val termOrdering: Ordering[Term] = TermOrdering

 
 
 @inline
 implicit class TermSymbolApply(s: Symbol)
 {
   def apply(args:Term*): Term =
     new FreeComplexTerm(SymbolName(s),args.toIndexedSeq)
 }
 
}
