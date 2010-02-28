package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;

/**
 * class represent variable, bound in eta-term.
 **/
class EtaXTerm(n: Name, l:Int, t: Term, o:EtaTerm, s:EtaXTermSignature) 
                                            extends Term
                                               with SimpleUnifyWithoutVM
                                               with SimpleSubst
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
   def subterm(i:Int) = None;

   /**
    * None
    **/
   def subterm(name:Name) = None;

   /**
    * Seq.empty
    */
   def subterms = RandomAccessSeq.empty;

   override def xOwner = if (owner==null) None else Some(owner);

   override def termUnify(t:Term, s:Substitution) : (Boolean, Substitution) = {
      if (t.isX) {
        if (t.xOwner.get == xOwner.get) {
            return (t.xLabel == xLabel,s);
        } else {
            val pair = (this,t);
            return s+(this,t);
        }
      } else {
        return s+(this,t);
      }
   }

   def termCompare(t:Term):Int = {
      var c = termClassIndex - t.termClassIndex;
      if (c!=0) return c;
      c = xLabel - t.xLabel;
      if (c!=0) return c;
      c=vname.compareTo(t.name);
      if (c!=0) return c;
      return owner.compareTo(t.xOwner.get);
   }

   override def termSubst(s:PartialFunction[Term,Term]):Term = {
      if (s.isDefinedAt(this)) s(this) else this;
   }
   
   override def isError: Boolean = false;
   override def isEta: Boolean = false;
   override def isAtom: Boolean = false;
   override def isNil: Boolean = false;

   override def termClassIndex: Int = TermClassIndex.ETA_X;

   override def termHashCode: Int = n.hashCode+xLabel+owner.hashCode*31;

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
