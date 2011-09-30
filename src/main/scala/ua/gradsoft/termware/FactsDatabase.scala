package ua.gradsoft.termware;

/**
 * facts database:
 *   this is set of signals and actions (i.e. input/output)
 **/
trait FactsDatabase
{

   /**
    * resolve term with name <code> n </code> and arity <code> a </code>  to input signal.
    **/
   def resolveSignal(n:Name,a:Int): Option[(Seq[Term],STMSubstitution[Term])=>(Boolean,STMSubstitution[Term])];

   /**
    * resolve term with  name <code> n </code> and arity <code> a </code>  to output action.
    **/
   def resolveAction(n:Name,a:Int): Option[(Seq[Term],STMSubstitution[Term])=>Unit];

}
