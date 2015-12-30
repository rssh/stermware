package termware


trait AtomTermOps extends TermEmptyComponents 
{

  this: AtomTerm =>

  override def isScoped = true

  override def isScope = false

  override def scopeIndex = -1

  override def isVar = false

  override def varIndex = -1

  override def withAttributes(newAttributes: Map[Name,Term]) =
     copy(attributes = newAttributes)
}

