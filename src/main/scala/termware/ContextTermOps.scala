package termware

trait ContextTermOps extends TermOps
{
  this: ContextTerm =>

  override def name = body.name

  override def cardinality = body.cardinality

}
