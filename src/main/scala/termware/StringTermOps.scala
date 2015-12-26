package termware


trait StringTermOps extends PrimitiveTermOps
{

  this: StringTerm =>

  override def withAttributes(newAttributes: Map[Name,Term]) =
     copy(attributes = newAttributes)
}
