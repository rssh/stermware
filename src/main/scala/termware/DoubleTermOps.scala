package termware

trait DoubleTermOps extends NumericTermOps
{
  this: DoubleTerm =>

  override val name = DoubleName(value)


}
