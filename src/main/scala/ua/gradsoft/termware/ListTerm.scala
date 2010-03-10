package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import ua.gradsoft.termware.fn._;

case class ListTerm(h:Term, t:Term, s:ListTermSignature) 
                                        extends FunctionalTerm(s)
{

  def arity: Int = 2;

  def subterm(i:Int):Option[Term] = 
    i match {
      case 0 => Some(head)
      case 1 => Some(tail)
      case _ => None
    }

  def subterms:RandomAccessSeq[Term] = RandomAccessSeq[Term](head,tail);

  override def termUnifyFn(t:Term, s: Substitution): (VM=>VM) =
   (vm:VM) => {
     if (patternName==t.patternName && t.arity==2) {
         val marker = vm.createAndPushMarker;
         vm.pushData(s);
         val tfrs = t.subterm(0).get;
         val tsnd = t.subterm(1).get;
         vm.pushCommandsReverse(
            (vm:VM) => { 
              val s = vm.popData.asInstanceOf[Substitution]; 
              head.termUnifyFn(tfrs,s)(vm); 
            },
            FnIfPop(
                FnPopData, // substitution now leave on stack
                FnSeqReverse(FnFalse,FnUpToMarker(marker))
            ),
            (vm:VM) => { 
              val s = vm.popData.asInstanceOf[Substitution]; 
              tail.termUnifyFn(tsnd,s)(vm); 
            },
            FnIfPop(
                FnPopData, // substitution now leave on stack
                FnSeqReverse(FnFalse,FnUpToMarker(marker))
            ),
            FnTrue
         );
     } else {
         vm.pushData(s);        
         vm.pushData(false);        
     }
     vm;
  }

  override def termSubstFn(s: PartialFunction[Term,Term]): (VM=>VM) = {
   (vm:VM) => {
       vm.pushCommand(signature.createTermFn(name,arity));
       vm.pushCommand(head.termSubstFn(s));
       vm.pushCommand(tail.termSubstFn(s));
       vm
   }
  }

  override def termCompare(t:Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    c = arity - t.arity;
    if (c!=0) return c;
    c = name.compareTo(t.name);
    if (c!=0) return c;
    c = head.termCompare(t.subterm(0).get);
    if (c!=0) return c;
    return tail.termCompare(t.subterm(1).get);
  }

  def name = signature.fixedName.get;

  lazy val termHashCode = name.hashCode+head.hashCode+tail.hashCode;
  
  val head = h;
  val tail = t;

}
