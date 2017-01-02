package termware

import language.implicitConversions

sealed trait TermCardinality

case class IntTermCardinality(v:Int) extends TermCardinality

case object FiniteTermCardinality extends TermCardinality

case object UniversumTermCardinality extends TermCardinality

case object ErrorTermCardinality extends TermCardinality

object TermCardinality
{
   implicit def int2TermCardinality(x:Int):TermCardinality = IntTermCardinality(x)

   final val ONE = IntTermCardinality(1)
   final val ZERO = IntTermCardinality(0)

   final val FINITE = FiniteTermCardinality
   final val UNIVERSUM = UniversumTermCardinality
   final val ERROR = ErrorTermCardinality

}
