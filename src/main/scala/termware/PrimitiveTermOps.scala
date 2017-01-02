package termware

trait PrimitiveTermOps extends TermOps
{

  this: PrimitiveTerm => 

  override def cardinality = TermCardinality.ONE

  override def arity: Int = 0


}
