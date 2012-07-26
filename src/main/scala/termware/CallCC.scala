package termware;

import scala.reflect.runtime.universe._;
import scala.collection.parallel._;


object CallCC
{

  def trampoline[A:TypeTag](cb:ComputationBounds[A]):A = {
       var current = cb;
       while (!current.isDone) {
         current = doStep(current)(CallNesting.empty);
       }
       current.result.get
  }

  def doStep[A:TypeTag](cb:ComputationBounds[A])(nesting:CallNesting):ComputationBounds[A] =
  {
    if (cb.isDone) {
        cb
    } else {
       try {
         cb.step(nesting) 
       } catch {
         case ex : CallCCThrowable[_] if (ex.aType <:< typeOf[A])
              => ex.current.asInstanceOf[ComputationBounds[A]]
       }
    }
  }

  def compose[A:TypeTag,B:TypeTag](ca:ComputationBounds[A],
                   cont:(A,CallNesting)=>ComputationBounds[B])
                   (nesting: CallNesting):
                                               ComputationBounds[B] = 
       nesting.withCall { nesting =>
         val s = doStep(ca)(nesting);
         if (s.isDone) {
            cont(s.result.get,nesting)
         } else {
            Call { nesting => compose(s,cont)(nesting) }
         }
       }

  def fcompose[A:TypeTag,B:TypeTag](ca:ComputationBounds[A],
                    cont:(A,CallNesting)=>ComputationBounds[B]):
                                                 ComputationBounds[B]=
    Call { nesting => compose(ca,cont)(nesting); }

  @inline
  def compose[A:TypeTag,B:TypeTag](ca:ComputationBounds[A],
                   cont:A=>ComputationBounds[B])
                   (nesting:CallNesting):    ComputationBounds[B] = 
    compose(ca,{ (x:A, nesting:CallNesting) => cont(x)})(nesting);


  def pair[A:TypeTag,B:TypeTag](ca:ComputationBounds[A],cb:ComputationBounds[B])
                                          (nesting:CallNesting):    ComputationBounds[(A,B)] =
     nesting.withCall { nesting =>
       // TODO" rewrite without seq (via future ?)
       val s = Seq(ca,cb).par.map( doStep(_)(nesting) )
       val cas = s.head.asInstanceOf[ComputationBounds[A]];
       val cbs = s.tail.head.asInstanceOf[ComputationBounds[B]];
       if (cas.isDone) {
         if (cbs.isDone) {
           Done((cas.result.get,cbs.result.get))
         } else {
           Call{ (nesting) => compose(cbs, { (b:B) => Done(cas.result.get,b) })(nesting) }
         }
       } else {
         if (cbs.isDone) {
           Call{ (nesting) => compose(cas, { (a:A) => Done(a,cbs.result.get) })(nesting) }
         } else {
           Call{ (nesting) => pair(cas,cbs)(nesting) }
         }
       }
     }

  def seqp[A:TypeTag](l:IndexedSeq[ComputationBounds[A]]) 
                         (nesting:CallNesting):ComputationBounds[IndexedSeq[A]] =  
     compose( Call{ nesting => seqp(l.par)(nesting) }, (x:ParSeq[A]) => Done(x.toIndexedSeq) )(nesting)


  def seqp[A:TypeTag](l:ParSeq[ComputationBounds[A]])(nesting:CallNesting):ComputationBounds[ParSeq[A]] =
   { val s1 = l.map(doStep(_)(nesting))
     if (s1.forall(_.isDone)) {
        Done(s1.map(_.result.get))
     } else {
        Call{ (nesting) => seqp(s1)(nesting) }
     }
   }

  def onProgress[A:TypeTag](ca:ComputationBounds[A],nesting:CallNesting)(action:CallNesting=>Unit):
                                                              ComputationBounds[A]=
  {
    compose(ca,{ (x:A, nesting: CallNesting) => action(nesting); Done(x) })(nesting);
  }


  final val MAX_NESTING=100;

}


