package termware


trait AtomTermOps extends TermEmptyComponents 
{

  this: AtomTerm =>

  override def isScoped = true

  override def scope = None 

  override def isVar = false

  override def withAttributes(newAttributes: Map[Name,Term]) =
     copy(attributes = newAttributes)
}

