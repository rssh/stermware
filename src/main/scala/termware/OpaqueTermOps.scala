package termware


trait OpaqueTermOps extends PrimitiveTermOps
{

  this: OpaqueTerm =>

  override def withAttributes(newAttributes: Map[Name,Term]) = copy(attributes = newAttributes)
}
