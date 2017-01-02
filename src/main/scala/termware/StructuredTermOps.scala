package termware

trait StructuredTermOps extends TermOps
{
  this: StructuredTerm =>

  override val name = termStructure.name

  override def cardinality: TermCardinality = TermCardinality.ONE

  override def arity: Int = components.length

}
