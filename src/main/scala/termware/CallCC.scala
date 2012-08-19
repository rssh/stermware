package termware;

import scala.reflect.runtime.universe._;
import scala.collection.parallel._;


object CallCC
{

  def trampoline[A](cb:ComputationBounds[A]):A = {
       val trampolineId = TrampolineId.next
       var current = cb;
       while (!current.isDone) {
         current = try {
           current.step(trampolineId, 0) 
         } catch {
           case ex : CallCCThrowable[_] if (ex.trampolineId == trampolineId)
              => ex.current.asInstanceOf[ComputationBounds[A]]
         }
       }
       current.result.get
  }

  def doStep[A](cb:ComputationBounds[A])(trampolineId: TrampolineId, depth:Int):ComputationBounds[A] =
  {
    if (cb.isDone) {
        cb
    } else {
       try {
         cb.step(trampolineId, depth) 
       } catch {
         case ex : CallCCThrowable[_] if (ex.trampolineId == trampolineId)
              => ex.current.asInstanceOf[ComputationBounds[A]]
       }
    }
  }

  // TODO:  check case when ca is composed
  @inline
  def compose[A,B](ca:ComputationBounds[A],
             cont:(TrampolineId,Int,A)=>ComputationBounds[B]): ComputationBounds[B] =
       Compose(ca,ContOne(cont))         


  @inline
  def compose[A,B](ca:ComputationBounds[A], cont:A=>ComputationBounds[B]): ComputationBounds[B] =
    compose(ca,{ (tid:TrampolineId, nesting:Int, x:A) => cont(x)});


  def pair[A,B](ca:ComputationBounds[A],cb:ComputationBounds[B])
                (tid:TrampolineId, nesting:Int):    ComputationBounds[(A,B)] =
     if (nesting > MAX_NESTING) {
         throw new CallCCThrowable(tid,Call{ (tid,n) => pair(ca,cb)(tid, n) })
     } else {
       // TODO" rewrite without seq (via future ?)
       val s = Seq(ca,cb).par.map( doStep(_)(tid,nesting) )
       val cas = s.head.asInstanceOf[ComputationBounds[A]];
       val cbs = s.tail.head.asInstanceOf[ComputationBounds[B]];
       if (cas.isDone) {
         if (cbs.isDone) {
           Done((cas.result.get,cbs.result.get))
         } else {
           compose(cbs, { (b:B) => Done(cas.result.get,b) }).step(tid,nesting)
         }
       } else {
         if (cbs.isDone) {
           compose(cas, { (a:A) => Done(a,cbs.result.get) }).step(tid,nesting)
         } else {
           Call{ (tid,nesting) => pair(cas,cbs)(tid,nesting) }
         }
       }
     }

  def seqp[A](l:IndexedSeq[ComputationBounds[A]]) 
                         (tid:TrampolineId, nesting:Int):ComputationBounds[IndexedSeq[A]] =  
     compose( Call{ (tid, nesting) => seqp(l.par)(tid, nesting) }, 
              (x:ParSeq[A]) => Done(x.toIndexedSeq) 
            ).step(tid,nesting)

  def seqp[A](l:ParSeq[ComputationBounds[A]])(tid:TrampolineId, nesting:Int):ComputationBounds[ParSeq[A]] =
   { val s1 = l.map(doStep(_)(tid,nesting))
     if (s1.forall(_.isDone)) {
        Done(s1.map(_.result.get))
     } else {
        Call{ (tid,nesting) => seqp(s1)(tid,nesting) }
     }
   }


  final val MAX_NESTING=500;


}


