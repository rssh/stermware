package termware

trait TermOps
{
  this: Term =>

  def name: Name

  def arity: Int

  def componentName(i: Int): Option[Name]

  def componentIndex(n: Name): Option[Int]

  def component(i: Int): Option[Term]

  def component(n: Name): Option[Term]

  def isScope: Boolean

  def scopeArity: Int

  def scopeVar(i: Int): Option[Term]

  def scopeVar(n: Name): Option[Term]

  def scopeVarName(i: Int): Option[Name]

  def scopeVarIndex(n: Name): Option[Int]

  def isScoped: Boolean

  def resolve(t: Term): Option[Term]

  def scopeIndex: Int

  def isVar: Boolean

  def varIndex: Int

  def withAttributes(attributes: Map[Name,Term]): Term

}
