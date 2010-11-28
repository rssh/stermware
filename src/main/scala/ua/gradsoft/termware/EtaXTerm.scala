package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import java.io.PrintWriter;
import ua.gradsoft.termware.flow._;

/**
 * class represent variable, bound in eta-term.
 **/
class EtaXTerm(n: Name, l:Int, t: Term, o:EtaTerm, s:EtaXTermSignature) 
                                            extends Term
                                               with ComplexUnify
                                               with SimpleSubst
                                               with SimpleCompare
                                               with NonNumberTerm
{

   /**
    * get name of EtaXTerm.
    *Note, that different eta-x terms can have same names.
    **/
   def name:Name = vname;

   /**
    * arity of eta-x term is always 0
    **/
   def arity:Int = 0;

   /**
    * None.
    **/
   def subterm(i:Int) = throwUOE;

   /**
    * None
    **/
   def subterm(name:Name) = throwUOE;

   /**
    * Seq.empty
    */
   def subterms = IndexedSeq.empty;

   override def xOwner = owner;

   override def unify(t:Term, s:Substitution)(implicit ctx:CallContext) 
                              : ComputationBounds[(Boolean, Substitution)] = {
      if (t.isX && (t.xOwner eq xOwner) ) {
            Done((t.xLabel == xLabel,s));
      } else {
        s+(this,t);
      }
   }

   def fixTermEq(t:Term): Boolean = { t.isX && (t.xOwner eq xOwner) }; 

   def fixTermCompare(t:Term):Int = {
      var c = termClassIndex - t.termClassIndex;
      if (c!=0) return c;
      c = xLabel - t.xLabel;
      if (c!=0) return c;
      c=vname.compareTo(t.name);
      if (c!=0) return c;
      return owner.compareTo(t.xOwner);
   }


   override def fixSubst(s:PartialFunction[Term,Term]):Term = {
      if (s.isDefinedAt(this)) s(this) else this;
   }
   
   override def isError: Boolean = false;
   override def isEta: Boolean = false;
   override def isAtom: Boolean = false;
   override def isNil: Boolean = false;

   override def termClassIndex: Int = TermClassIndex.ETA_X;

   override def termHashCode: Int = n.hashCode+xLabel+owner.hashCode*31;

   override def print(out:PrintWriter):Unit = { out.print(name.string); }

   private[termware] def setOwner(o:EtaTerm): Unit = {
     owner=o;
   }

   

   private[termware] val vname: Name = n;
   override val xLabel: Int = l;
   private[termware] var owner: EtaTerm = o;
            val signature = s;
            val typeTerm = t;

   lazy val attributes = new HashMap[Name,Term];

}
