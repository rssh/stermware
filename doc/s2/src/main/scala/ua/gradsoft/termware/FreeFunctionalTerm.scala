package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import java.io.PrintWriter;


class FreeFunctionalTerm(n:Name,ts:IndexedSeq[Term],
                               s:FunctionalTermSignature) 
                                     extends FunctionalTerm(s)
{
	
	

  def arity: Int = subterms.length;

  def subterm(i:Int):Term = subterms(i);
    
  lazy val termHashCode = name.hashCode+subterms.
                              foldLeft(0)((x:Int,y:Term)=>x+y.termHashCode);

  val name=n;
  val subterms=ts;

}

object FreeFunctionalTerm {
  
  def apply(n:Name, ts:IndexedSeq[Term], s:FunctionalTermSignature) =
          new FreeFunctionalTerm(n,ts,s);

}
