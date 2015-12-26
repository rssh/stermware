package termware

import util._

trait TermSerializer
{

   def apply(t: Term, out: Output): Unit

   def unapply(input: Input): Term

}
