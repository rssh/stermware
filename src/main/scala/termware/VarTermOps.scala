package termware

trait VarTermOps extends TermEmptyComponents
{
   this: Term =>

   override def isVar = true

   override def isScoped = scope.isDefined

}
