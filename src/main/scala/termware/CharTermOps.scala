package termware

trait CharTermOps extends TermOps
{
  this: CharTerm =>

  override val name = CharName(value)

}
