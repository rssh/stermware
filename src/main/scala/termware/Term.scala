package termware


sealed trait Term extends TermOps
{
 val attributes: Map[Name,Term] = Map()
 val termSystem: TermSystem = FreeTermSystem
}

class AtomTerm(value:String, 
               override val attributes: Map[Name,Term] = Map(), 
               override val termSystem: TermSystem = FreeTermSystem) extends Term with AtomTermOps
{
  val name = AtomName(value)
}

sealed trait PrimitiveTerm extends Term with PrimitiveTermOps 

class StringTerm(value:String) extends PrimitiveTerm with StringTermOps
{
  val name = StringName(value)
}

class CharTerm(value:Character) extends PrimitiveTerm with CharTermOps
{
 val name = CharName(value)
}

sealed trait NumericTerm extends PrimitiveTerm with NumericTermOps

class Int32Term(value:Int) extends NumericTerm with Int32TermOps
{
  val name = IntName(value)
}

class Int64Term(value: Long) extends NumericTerm with Int64TermOps
{
  val name = LongName(value)
}

class DoubleTerm(value: Double) extends NumericTerm with DoubleTermOps
{
  val name = DoubleName(value)
}

case class OpaqueTerm(value: Array[Byte]) extends PrimitiveTerm with OpaqueTermOps
{
  val name = OpaqueName(value)
}

case class StructuredTerm(termStructure: TermStructure, 
                          components: IndexedSeq[Term],
                          override val scope: Option[Term] = None,
                          override val attributes: Map[Name,Term] = Map(), 
                          override val termSystem: TermSystem = FreeTermSystem
                          ) extends Term with StructuredTermOps

class VarTerm(val name: Name,
              override val scope: Option[Term] = None,
              override val attributes: Map[Name,Term] = Map(), 
              override val termSystem: TermSystem = FreeTermSystem
             ) extends Term with VarTermOps


