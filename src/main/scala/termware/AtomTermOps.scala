package termware


trait AtomTermOps extends TermEmptyComponents with UnattributedTermOps
{

  this: AtomTerm =>

  def isFreeVar = false

}

