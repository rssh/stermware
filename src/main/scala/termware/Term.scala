package termware


sealed trait Term extends TermOps

sealed trait UniTerm extends Term with UniTermOps

case class AtomTerm(value:String) extends UniTerm with AtomTermOps

sealed trait PrimitiveTerm extends UniTerm with PrimitiveTermOps 

case class StructuredTerm(
                          termStructure: TermStructure, 
                          components: IndexedSeq[Term]
                          ) extends UniTerm with StructuredTermOps

object StructuredTerm
{
    def apply(n:Name, components: IndexedSeq[Term]): StructuredTerm =
          StructuredTerm(SeqTermStructure(n),components)
}

case class ContextTerm(
                    scope: SetTerm,
                    body: Term 
                    ) extends Term with ContextTermOps 

class SetTerm(args: Seq[UniTerm])  extends Term with SetTermOps
class EmptySetTerm extends Term with EmptySetTermOps

case class ArrowTerm(x:Term, y:Term)  extends Term with ArrowTermOps

class UniversumTerm extends Term with UniversumTermOps

class ErrorTerm extends Term with ErrorTermOps

// Primitive terms
case class StringTerm(value:String) extends PrimitiveTerm with StringTermOps
case class CharTerm(value:Character) extends PrimitiveTerm with CharTermOps
sealed trait NumericTerm extends PrimitiveTerm with NumericTermOps
case class Int32Term(value:Int) extends NumericTerm with Int32TermOps
case class Int64Term(value: Long) extends NumericTerm with Int64TermOps
case class DoubleTerm(value: Double) extends NumericTerm with DoubleTermOps
case class OpaqueTerm(value: Array[Byte]) extends PrimitiveTerm with OpaqueTermOps
