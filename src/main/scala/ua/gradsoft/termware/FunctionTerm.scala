package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import ua.gradsoft.termware.fn._;

case class FunctionalTerm(n:Name,ts:RandomAccessSeq[Term],
                                 s:FunctionalTermSignature) 
                                        extends Term
                                          with ComplexUnify
                                          with ComplexSubst
{


  def isNil: Boolean = false;
  def isAtom: Boolean = false;
  def isEta: Boolean = false;
  def isError: Boolean = false;

  def arity: Int = subterms.length;

  def subterm(i:Int):Option[Term] = 
    if (i<subterms.length) Some(subterms(i)) else None
  


  def termUnifyFn(t:Term, s: Substitution): (VM=>VM) =
   (vm:VM) => {
     if (patternName==t.patternName && arity==t.arity) {
         val marker = vm.createAndPushMarker;
         vm.pushData(s);
         for(i<- 0 to arity) {
           val st1 = subterm(i).get;
           val st2 = t.subterm(i).get;
           vm.pushCommandsReverse(
              (vm:VM) => { 
                 val s = vm.popData.asInstanceOf[Substitution]; 
                 st1.termUnifyFn(st2,s);
                 vm;
              },
              FnIfPop(
                FnPopData, // substitution now leave on stack
                FnSeqReverse(FnFalse,FnUpToMarker(marker))
              )
           ); 
         }
         // if we here, than all ok,
         vm.pushCommandsReverse(FnTrue);
     } else {
         vm.pushData(s);        
         vm.pushData(false);        
     }
     vm;
  }

  def termSubstFn(s: PartialFunction[Term,Term]): (VM=>VM) = {
   (vm:VM) => {
       vm.pushCommand(signature.createTermFn(name,arity));
       for(i<- 0 to arity) {
         val st1 = subterm(arity-i-1).get;
         vm.pushCommand(st1.termSubstFn(s));
       }
       vm
   }
  }

  def termClassIndex = TermClassIndex.FUNCTIONAL;

  def termCompare(t:Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    c = arity - t.arity;
    if (c!=0) return c;
    c = name.compareTo(t.name);
    return c; 
  }

  lazy val termHashCode = n.hashCode+subterms.
                              foldLeft(0)((x:Int,y:Term)=>x+y.termHashCode);
  
  val name=n;
  val subterms=ts;
  val signature=s;
  val attributes = new HashMap[Name,Term]();

}
