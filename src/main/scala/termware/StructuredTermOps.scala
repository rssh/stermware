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

  override def varIndex = -1

  override def scopeIndex = -1

  override def scopeArity = 0

  override def isScoped = false

  override def resolve(t: Term) = None

  override def isScope = false

  override def scopeVar(n: Name): Option[Term] = None

  override def scopeVar(i: Int): Option[Term] = None

  override def scopeVarIndex(n: Name): Option[Int] = None

  override def scopeVarName(i: Int): Option[Name] = None

  override def withAttributes(newAttributes: Map[Name,Term]) = copy(attributes = newAttributes)

}
