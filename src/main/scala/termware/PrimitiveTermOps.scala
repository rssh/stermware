package termware


trait PrimitiveTermOps extends UnattributedTermOps
{
  this: PrimitiveTerm =>

  def arity = 0

  def component(n:Name) = None

  def component(i:Int) = None

  def componentNames : IndexedSeq[Name] = IndexedSeq()

  def isFreeVar = false

}
