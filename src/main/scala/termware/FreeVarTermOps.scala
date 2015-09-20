package termware

trait FreeVarTermOps extends TermEmptyComponents with UnattributedTermOps
{

  this: FreeVarTerm =>

  def isFreeVar: Boolean = true

}
