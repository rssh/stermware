package termware


trait StructuredTermOps extends TermOps
{

  this: StructuredTerm =>

  override def arity = components.length

  override def name = termStructure.name

  override def componentName(i:Int) = termStructure.componentName(i)

  override def componentIndex(n:Name) = termStructure.componentIndex(n)

  override def component(i:Int) = if (i < components.length) {
                            Some(components(i))
                         } else None

  override def component(n:Name) = termStructure.componentIndex(n) map(components(_))

  override def isVar: Boolean = false

  override def scope = None

  override def isScoped = false

}
