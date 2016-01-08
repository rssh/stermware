package termware.free

import termware._

sealed trait FreeTermContext extends TermContext
{

 override def resolve(source: Term, name: Term): TrackedResult[Term] = TrackedFailure("not iplemented")

}

case class TopLevelTermContext(override val termSystem: TermSystem) extends FreeTermContext
{

 def childContext(term: Term,parent: TermWithContext):TermContext = 
   ChildTermContext(termSystem, parent)
 
 def scope(source: Term, index: Int): Term = 
   if (source.isScope && source.scopeIndex == index) {
      source
   } else {
      throwPE(s"scope ${index} is not not found")
   }

}

case class ChildTermContext(override val termSystem: TermSystem, val parent: TermWithContext) extends FreeTermContext
{

 def childContext(term: Term,parent: TermWithContext):TermContext = 
   ChildTermContext(termSystem, parent)

 def scope(source: Term, index: Int): Term = 
   if (source.isScope && source.scopeIndex == index) {
      source
   } else {
      parent.context.scope(parent.term, index)
   }


}

