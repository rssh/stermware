package termware

trait UniTermOps extends TermOps
{

  this: UniTerm =>

  override def cardinality: TermCardinality  = TermCardinality.ONE

  def arity: Int

  def uniTerm(): UniTerm = this

}
