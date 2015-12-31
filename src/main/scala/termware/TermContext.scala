package termware

trait TermContext
{

   def scope(source: Term, index:Int):Term

   def termSystem(): TermSystem

 //  def variable(scopeIndex:Int, varIndex:Int):VarTerm

 //   def resolve(name:Name):Option[VarTerm]

   def childContext(term: Term, parent: TermWithContext): TermContext

}
