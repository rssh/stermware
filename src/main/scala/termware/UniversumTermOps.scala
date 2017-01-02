package termware

trait UniversumTermOps extends TermOps
{

  this: UniversumTerm =>

  override def name = UniversumName

  override def cardinality = TermCardinality.UNIVERSUM

}
