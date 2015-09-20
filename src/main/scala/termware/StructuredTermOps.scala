package termware


trait StructuredTermOps extends UnattributedTermOps
{

  this: StructuredTerm =>

  def arity = components.length

  def name = termStructure.name

  def componentNames = termStructure.componentNames

  def component(i:Int) = if (i < components.length) {
                            Some(components(i))
                         } else None

  def component(n:Name) = termStructure.componentIndexes.get(n) map(components(_))

  def isFreeVar: Boolean = false

}
