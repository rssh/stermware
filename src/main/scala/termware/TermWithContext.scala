package termware

case class TermWithContext(term: Term, context: TermContext)
{

   def scope(): Option[Term] = 
    if (term.scopeIndex == -1) None else Some(context.scope(term,term.scopeIndex))

   def name: Name = term.name

   def arity: Int = term.arity

   def components(): IndexedSeq[TermWithContext] = 
   {
     term match {
       case st: StructuredTerm => 
                   st.components map wrapChild
       case _ => IndexedSeq()
     }
   }

   def component(n:Name): Option[TermWithContext] =
        term.component(n) map wrapChild

   def component(i:Int): Option[TermWithContext] =
        term.component(i) map wrapChild


   private def wrapChild(c: Term): TermWithContext =
        TermWithContext(c, context.childContext(c,this))

}


