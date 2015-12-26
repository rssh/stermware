package termware

trait Unification
{

  def apply(x: Term, y: Term, scope:Term): Match

}
