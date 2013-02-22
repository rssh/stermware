package termware

import scala.util._

trait TermSignature
{
 
   def subst(t:Term, s:Substitution): Term

   def unify(x:Term, y:Term): Try[Substitution]


}
