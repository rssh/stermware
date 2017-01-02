package termware

trait TermOps
{
  this: Term =>

  def name: Name 

  def cardinality: TermCardinality

}
