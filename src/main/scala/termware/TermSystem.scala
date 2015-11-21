package termware


trait TermSystem
{

   def name: Term

   def adopt(t: Term): Term

   def mergeAttributes(attrName: Name, x:Term, y:Term): Term

   def isFree: Boolean
}

