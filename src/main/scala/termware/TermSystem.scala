package termware


trait TermSystem
{

   def adopt(t: Term): Term

   def mergeAttributes(attrName: Name, x:Term, y:Term): Term

}

