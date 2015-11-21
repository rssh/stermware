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

  override def isScoped = false

  override def scope = None

}
