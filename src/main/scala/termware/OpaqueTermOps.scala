package termware

trait OpaqueTermOps extends PrimitiveTermOps
{
  this: OpaqueTerm =>

  override val name = OpaqueName(value)

}
