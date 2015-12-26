package termware


object FreeTermSystem extends TermSystem
{

   def name: Term = new AtomTerm("Free")

   override def adopt(t: Term): Term = t

   override def unification: Unification = free.Unification

   override def isFree: Boolean = true

}

