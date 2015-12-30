package termware

trait TermOps
{
  this: Term =>

  def name: Name 

  def arity: Int 

  def componentName(i:Int): Option[Name] 

  def componentIndex(n:Name): Option[Int] 

  def component(i: Int): Option[Term] 

  def component(n: Name): Option[Term] 

  def isScope: Boolean

  def isScoped: Boolean

  def scopeIndex: Int

  def isVar: Boolean

  def varIndex: Int

  def withAttributes(attributes: Map[Name,Term]): Term

}
