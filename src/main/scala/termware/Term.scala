package termware


sealed trait Term extends TermOps
{
 val attributes: Map[Name,Term] = Map()
}

case class AtomTerm(value:String, 
                    override val attributes: Map[Name,Term] = Map()
                   ) extends Term with AtomTermOps
{
  override val name = AtomName(value)
}



sealed trait PrimitiveTerm extends Term with PrimitiveTermOps 


case class StringTerm(value:String,
               override val attributes: Map[Name,Term] = Map()
                     ) extends PrimitiveTerm with StringTermOps
{
  override val name = StringName(value)
}

case class CharTerm(value:Character,
               override val attributes: Map[Name,Term] = Map()
                   ) extends PrimitiveTerm with CharTermOps
{
 override val name = CharName(value)
}

sealed trait NumericTerm extends PrimitiveTerm with NumericTermOps

case class Int32Term(value:Int,
                override val attributes: Map[Name,Term] = Map()
               ) extends NumericTerm with Int32TermOps
{
  override val name = IntName(value)
}

case class Int64Term(value: Long,
                override val attributes: Map[Name,Term] = Map()
               ) extends NumericTerm with Int64TermOps
{
  override val name = LongName(value)
}

case class DoubleTerm(value: Double,
                 override val attributes: Map[Name,Term] = Map()
                ) extends NumericTerm with DoubleTermOps
{
  override val name = DoubleName(value)
}

case class OpaqueTerm(value: Array[Byte],
                 override val attributes: Map[Name,Term] = Map() 
                     ) extends PrimitiveTerm with OpaqueTermOps
{
  override val name = OpaqueName(value)
}

// TODO: add min/max index.
case class StructuredTerm(
                          termStructure: TermStructure, 
                          components: IndexedSeq[Term],
                          override val attributes: Map[Name,Term] = Map()
                          ) extends Term with StructuredTermOps
{
}

object StructuredTerm
{
    def apply(n:Name, components: IndexedSeq[Term]): StructuredTerm =
          StructuredTerm(SeqTermStructure(n,false),components)
}

case class VarTerm(val name: Name,
                   val varIndex: Int,  // index in scope. 
              override val scopeIndex: Int = -1, // index of scope. point to 
              override val attributes: Map[Name,Term] = Map()
             ) extends Term with VarTermOps
{
}


