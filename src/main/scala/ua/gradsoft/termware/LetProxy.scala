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
               var letOwner :LetTerm) extends Term
                      with ProxyTerm
{


   override def termHashCode: Int = proxy.termHashCode;

   override def termCompare(t:Term)(implicit ctx:CallContext)
                                                :ComputationBounds[Int] =
      CallCC.compose(toComputationBounds,
          { (x:Term,ctx:CallContext) => x.termCompare(t)(ctx) }
      );

   override def subst(s: PartialFunction[Term,Term])(implicit ctx:CallContext):
                                                     ComputationBounds[Term]
    =
      CallCC.compose(toComputationBounds,
             { (x:Term,ctx:CallContext) => x.subst(s)(ctx) }
      );

   def print(out: PrintWriter): Unit =
   {  out.print(letOwner.vars(letLabel).name); }


   def proxy: Term = letOwner.vars(letLabel).value;

}

