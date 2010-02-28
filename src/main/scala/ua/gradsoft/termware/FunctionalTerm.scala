package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import ua.gradsoft.termware.fn._;

abstract class FunctionalTerm(s:TermSignature) extends Term
                                             with ComplexUnify
                                             with ComplexSubst
                                             with NonNumberTerm
{

  def isNil: Boolean = false;
  def isAtom: Boolean = false;
  def isEta: Boolean = false;
  def isError: Boolean = false;

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
       for(i<- 0 to arity-1) {
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
    if (c!=0) return c;
    var i=0;
    while(i<arity) {
       c=subterms(i).termCompare(t.subterms(i));
       if (c!=0) return c;
       i=i+1;  // scala have no ++i 
    }
    return c; 
  }

  
  val signature=s;
  lazy val attributes = new HashMap[Name,Term]();

}
