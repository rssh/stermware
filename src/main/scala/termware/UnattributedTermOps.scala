package termware

trait UnattributedTermOps 
{
  this: UnattributedTerm =>

  def isAttributed: Boolean = false

  def attributes: Map[Name,Term] = Map()

  def isSystemized: Boolean = false

  def termSystem: TermSystem = FreeTermSystem
}
