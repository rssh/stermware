package termware

case class TermWithContext(term: Term, context: TermContext)
{

   def scope(): Option[Term] = 
    if (term.scopeIndex == -1) None else Some(context.scope(term.scopeIndex))

   def arity(): Int = term.arity

   def components(): IndexedSeq[TermWithContext] = 
   {
     term match {
       case st: StructuredTerm => 
                   st.components map (x => TermWithContext(x, context.childContext(x,this) ))
       case _ => IndexedSeq()
     }
   }

}


