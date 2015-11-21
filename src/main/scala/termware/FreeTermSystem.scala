package termware


object FreeTermSystem extends TermSystem
{

   def name: Term = new AtomTerm("Free")

   def adopt(t: Term): Term = t

   def mergeAttributes(attrName: Name, x:Term, y:Term): Term = ???

   def isFree: Boolean = true

}

