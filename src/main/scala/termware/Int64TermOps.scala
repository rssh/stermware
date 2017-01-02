package termware

trait Int64TermOps extends NumericTermOps
{
  this: Int64Term =>

  override val name = LongName(value)

}
