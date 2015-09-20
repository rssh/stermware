package termware


sealed trait Term extends TermOps

sealed trait UnattributedTerm extends Term

class AtomTerm(value:String) extends UnattributedTerm with AtomTermOps
{
  val name = AtomName(value)
}

sealed trait AttributedTerm extends Term
{
 def unattributed: Term
 def attributes: Map[Name,Term]
}

class SimpleAttributedTerm(t: UnattributedTerm, a: Map[Name, Term]) extends AttributedTerm with AttributedTermOps
{
  def unattributed = t
  def attributes = a

  def isSystemized = false
}

sealed trait SystemizedTerm extends AttributedTerm

class SystemizedAttributedTerm(t: UnattributedTerm, a: Map[Name, Term], ts: TermSystem) extends AttributedTerm
                                                                                         with AttributedTermOps
{
  def unattributed = t
  def attributes = a

  def isSystemized = false
  def termSystem = ts
}


sealed trait PrimitiveTerm extends UnattributedTerm with PrimitiveTermOps with UnattributedTermOps

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


case class StructuredTerm(termStructure: TermStructure, components: IndexedSeq[Term]) extends UnattributedTerm with StructuredTermOps

class FreeVarTerm(val name:Name) extends UnattributedTerm with FreeVarTermOps


//class LetVarTerm extends Term


