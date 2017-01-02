package termware

trait EmptySetTermOps extends TermOps
{

  this: EmptySetTerm =>

  override def name = SetName

  override def cardinality = TermCardinality.ZERO

}
