package termware


object FreeTermSystem extends TermSystem
{

   def adopt(t: Term): Term = t

   def mergeAttributes(attrName: Name, x:Term, y:Term): Term = ???
     

}

