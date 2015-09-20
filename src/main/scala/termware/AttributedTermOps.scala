package termware

trait AttributedTermOps
{
  this: AttributedTerm =>

  def name: Name = unattributed.name

  def arity: Int = unattributed.arity

  def componentNames: IndexedSeq[Name] =
       unattributed.componentNames

  def component(i: Int): Option[Term] =
       unattributed.component(i)

  def component(n: Name): Option[Term] =
       unattributed.component(n)
       
  def isAttributed: Boolean = true

  def isFreeVar: Boolean = unattributed.isFreeVar

}
