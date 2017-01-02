package termware

trait ErrorTermOps extends TermOps
{
  this: ErrorTerm =>

  override def name = ErrorName

  override def cardinality = TermCardinality.ERROR
}
