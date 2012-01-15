package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import java.io.PrintWriter;
import ua.gradsoft.termware.flow._;

/**
 * class represent variable, bound in let-term.
 *(All methods are delegated to proxy)
 **/
case class LetProxy(val letName: Name, 
               val letLabel:Int, 
               val letOwner :LetTerm) extends Term
                      with ProxyTerm
{


   override def subst(s: PartialFunction[Term,Term])(implicit ctx:CallContext):
                                                     ComputationBounds[Term]
    =
      Done(if (s.isDefinedAt(this)) {
                s.apply(this)
           } else {
                this
           });

   def print(out: PrintWriter): Unit =
   {   
     if (letOwner==null) {
       out.print(this.toString);
     } else {
       out.print(letOwner.vars(letLabel).name); 
     }
   }


   def proxy: Term = letOwner.vars(letLabel).value;

}

