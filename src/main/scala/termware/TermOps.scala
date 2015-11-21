package termware

trait TermOps
{
  this: Term =>

  def name: Name 

  def arity: Int 

  def componentNames: IndexedSeq[Name] 

  def component(i: Int): Option[Term] 

  def component(n: Name): Option[Term] 

  def isScoped: Boolean

  def scope: Option[Term]

  def isVar: Boolean

}
