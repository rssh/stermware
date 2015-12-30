package termware


trait PrimitiveTermOps 
{
  this: PrimitiveTerm =>

  override def arity = 0

  override def component(n:Name) = None

  override def component(i:Int) = None

  override def componentName(i:Int) = None

  override def componentIndex(n:Name) = None

  override def isVar = false

  override def varIndex = -1

  override def isScoped = false

  override def isScope = false

  override def scopeIndex = -1


}
