package termware


trait StructuredTermOps extends TermOps
{

  this: StructuredTerm =>

  override def arity = components.length

  override def name = termStructure.name

  override def componentNames = termStructure.componentNames

  override def component(i:Int) = if (i < components.length) {
                            Some(components(i))
                         } else None

  override def component(n:Name) = termStructure.componentIndexes.get(n) map(components(_))

  override def isVar: Boolean = false

  override def isScoped = scope.isDefined

}
