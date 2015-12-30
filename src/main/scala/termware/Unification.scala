package termware

trait Unification
{

  def apply(x: TermWithContext, y: TermWithContext, scope:Term): Match

}
