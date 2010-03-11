package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import ua.gradsoft.termware.fn._;

class ListTerm(h:Term, t:Term, s:ListTermSignature) 
                                        extends FunctionalTerm(s)
{

  def arity: Int = 2;

  def subterm(i:Int):Term = 
    i match {
      case 0 => head
      case 1 => tail
      case _ => throw new IndexOutOfBoundsException();
    }

  def subterms:RandomAccessSeq[Term] = RandomAccessSeq[Term](head,tail);

  override def termUnifyFn(t:Term, s: Substitution): (VM=>VM) =
   (vm:VM) => {
     if (patternName==t.patternName && t.arity==2) {
         val marker = vm.createAndPushMarker;
         vm.pushData(s);
         val tfrs = t.subterm(0);
         val tsnd = t.subterm(1);
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
    c = head.termCompare(t.subterm(0));
    if (c!=0) return c;
    return tail.termCompare(t.subterm(1));
  }

  def name = signature.fixedName.get;

  override def toString = "[" + toStringI(this);

  def toStringI(t:Term):String = t match {
    case l:ListTerm => l.subterm(0).toString +
                              (if (l.subterm(1).isNil) {
                                 "]"
                               } else {
                                 "," +toStringI(l.subterm(1))
                               });
    case _ => t.toString
  }

  lazy val termHashCode = name.hashCode+head.hashCode+tail.hashCode;
  
  val head = h;
  val tail = t;

}
