package termware

trait VarTermOps extends TermEmptyComponents
{
   this: VarTerm =>

   override def isVar = true

   override def isScoped = scopeIndex != -1

   override def withAttributes(newAttributes: Map[Name,Term]) = copy(attributes = newAttributes)

}
