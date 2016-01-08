package termware

trait TermContext
{

   def termSystem(): TermSystem

   def scope(source: Term, index:Int):Term

   def resolve(source: Term, name:Term): TrackedResult[Term]

   def childContext(term: Term, parent: TermWithContext): TermContext

}
