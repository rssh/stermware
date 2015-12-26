package termware


sealed trait Term extends TermOps
{
 val attributes: Map[Name,Term] = Map()
 val termSystem: TermSystem = FreeTermSystem

}

case class AtomTerm(value:String, 
               override val attributes: Map[Name,Term] = Map(), 
               override val termSystem: TermSystem = FreeTermSystem
                   ) extends Term with AtomTermOps
{
  override val name = AtomName(value)

  override def withAttributes(newAttributes: Map[Name,Term]) =
     copy(attributes = newAttributes)

}

sealed trait PrimitiveTerm extends Term with PrimitiveTermOps 


case class StringTerm(value:String,
               override val attributes: Map[Name,Term] = Map(), 
               override val termSystem: TermSystem = FreeTermSystem
                     ) extends PrimitiveTerm with StringTermOps
{
  override val name = StringName(value)

  override def withAttributes(newAttributes: Map[Name,Term]) =
     copy(attributes = newAttributes)

}

case class CharTerm(value:Character,
               override val attributes: Map[Name,Term] = Map(), 
               override val termSystem: TermSystem = FreeTermSystem
                   ) extends PrimitiveTerm with CharTermOps
{
 override val name = CharName(value)

 override def withAttributes(newAttributes: Map[Name,Term]) =
     copy(attributes = newAttributes)

}

sealed trait NumericTerm extends PrimitiveTerm with NumericTermOps

case class Int32Term(value:Int,
                override val attributes: Map[Name,Term] = Map(), 
                override val termSystem: TermSystem = FreeTermSystem
               ) extends NumericTerm with Int32TermOps
{
  override val name = IntName(value)
  override def withAttributes(newAttributes: Map[Name,Term]) = copy(attributes = newAttributes)
}

case class Int64Term(value: Long,
                override val attributes: Map[Name,Term] = Map(), 
                override val termSystem: TermSystem = FreeTermSystem
               ) extends NumericTerm with Int64TermOps
{
  override val name = LongName(value)
  override def withAttributes(newAttributes: Map[Name,Term]) = copy(attributes = newAttributes)
}

case class DoubleTerm(value: Double,
                 override val attributes: Map[Name,Term] = Map(), 
                 override val termSystem: TermSystem = FreeTermSystem
                ) extends NumericTerm with DoubleTermOps
{
  override val name = DoubleName(value)
  override def withAttributes(newAttributes: Map[Name,Term]) = copy(attributes = newAttributes)
}

case class OpaqueTerm(value: Array[Byte],
                 override val attributes: Map[Name,Term] = Map(), 
                 override val termSystem: TermSystem = FreeTermSystem
                     ) extends PrimitiveTerm with OpaqueTermOps
{
  override val name = OpaqueName(value)
  override def withAttributes(newAttributes: Map[Name,Term]) = copy(attributes = newAttributes)
}

// TODO: add min/max index.
case class StructuredTerm(
                          termStructure: TermStructure, 
                          components: IndexedSeq[Term],
                          override val attributes: Map[Name,Term] = Map(), 
                          override val termSystem: TermSystem = FreeTermSystem
                          ) extends Term with StructuredTermOps
{
    override def withAttributes(newAttributes: Map[Name,Term]) = copy(attributes = newAttributes)
}

object StructuredTerm
{
    def apply(n:Name, components: IndexedSeq[Term]): StructuredTerm =
          StructuredTerm(FreeTermStructure(n),components)
}

case class VarTerm(val name: Name,
                   val index: Int,
              override val scope: Option[Term] = None,
              override val attributes: Map[Name,Term] = Map(), 
              override val termSystem: TermSystem = FreeTermSystem
             ) extends Term with VarTermOps
{
    override def withAttributes(newAttributes: Map[Name,Term]) = copy(attributes = newAttributes)
}


