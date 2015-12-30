package termware

trait TermContext
{

   def scope(index:Int):Term

 //  def variable(scopeIndex:Int, varIndex:Int):VarTerm

 //   def resolve(name:Name):Option[VarTerm]

   def childContext(term: Term, parent: TermWithContext): TermContext

}
