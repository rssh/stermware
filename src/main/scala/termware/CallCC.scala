package termware;

import scala.reflect.runtime.universe._;


object CallCC
{

  def trampoline[A:TypeTag](cb:ComputationBounds[A]):A = {
       var quit = false;
       var result: Option[A] = None;
       var current = cb;
       while(!quit) {
         if (current.isDone) {
             result=current.result;
             quit=true;
         } else {
             try {
               current = current.step(CallNesting.empty);
             } catch {
                case ex : CallCCThrowable[_] if (ex.aType <:< typeOf[A])
                    => current = ex.current.asInstanceOf[ComputationBounds[A]]
             }
         }
       }
       return result.get;
  }

  def compose[A,B](ca:ComputationBounds[A],
                  cont:(A,CallNesting)=>ComputationBounds[B])
                (implicit ctx:CallNesting,aTag:TypeTag[A],bTag:TypeTag[B]):
                                               ComputationBounds[B] = 
       ctx.withCall {
         (ctx:CallNesting)=> implicit val ictx = ctx; 
         var current = ca;
         var quit = false;
         var result: Option[ComputationBounds[B]] = None;
         while(!quit) {
           try {
             current=current.step(ctx);  
             if (current.isDone) {
               result=Some(cont(current.result.get,ctx));
               quit=true;
             }
           }catch{
             case ex:CallCCThrowable[_] if (ex.aType <:< ca.aType)
                  => current=ex.current.asInstanceOf[ComputationBounds[A]];
             if (current.isDone) {
                val sa = cont(current.result.get,ctx);
                throw new CallCCThrowable[B](sa);
             } else {
                throw new CallCCThrowable[B](Call{ (ctx:CallNesting)=> 
                                                    compose(ca,cont)(ctx,aTag,bTag); });
             }
           }
         } 
         result.get;
       }

  def fcompose[A:TypeTag,B:TypeTag](ca:ComputationBounds[A],
                    cont:(A,CallNesting)=>ComputationBounds[B]):
                                                 ComputationBounds[B]=
    Call { implicit ctx => compose(ca,cont); }

  @inline
  def compose[A,B](ca:ComputationBounds[A],
                   cont:A=>ComputationBounds[B])
                   (implicit ctx:CallNesting, aTag: TypeTag[A], bTag: TypeTag[B]):
                                               ComputationBounds[B] = 
    compose(ca,{ (x:A,ctx:CallNesting) => cont(x) });


  def pair[A,B](ca:ComputationBounds[A],cb:ComputationBounds[B])
               (implicit ctx:CallNesting, aTag: TypeTag[A], bTag: TypeTag[B]):
                                           ComputationBounds[(A,B)] =
     ctx.withCall {
       implicit ctx =>  
       var cca = ca; 
       var ccb = cb; 
       var quit=false;
       while(!quit) {
          if (!cca.isDone) {
            try {
              cca=cca.step(ctx)
            } catch {
              case ex: CallCCThrowable[_] if ex.aType <:< typeOf[A] => {
                cca = ex.current.asInstanceOf[ComputationBounds[A]];
                throw new CallCCThrowable(
                   Call{ implicit ctx:CallNesting => pair(cca,ccb) });
              }
            }
          } else if (ccb.isDone) {
            quit = true;
          }
          if (!ccb.isDone) {
            try {
              ccb=ccb.step(ctx)
            } catch {
              case ex: CallCCThrowable[_] if ex.aType <:< typeOf[B] => {
                ccb = ex.current.asInstanceOf[ComputationBounds[B]];
                throw new CallCCThrowable(
                    Call{ implicit ctx:CallNesting => pair(cca,ccb); });
              }
            }
          }
       }
       Done((cca.result.get,ccb.result.get)); 
   }

   def tuple[A:TypeTag,B:TypeTag](ca:ComputationBounds[A],cb:ComputationBounds[B])
                                                        (implicit ctx:CallNesting) = pair(ca,cb);
              


   def tuple[A:TypeTag,B:TypeTag,C:TypeTag](ca:ComputationBounds[A],
                                            cb:ComputationBounds[B], 
                                            cc:ComputationBounds[C])(
                                              implicit ctx:CallNesting):ComputationBounds[(A,B,C)] =
                compose(pair(ca,pair(cb,cc)),
                        { (x:(A,(B,C))) => Done((x._1, x._2._1, x._2._2)) }
                       );
                         

   @inline
   def reduce[A](cba: ComputationBounds[A])(implicit ctx:CallNesting)
                                         :(ComputationBounds[A],Boolean) = {
      var inex = false;
      var current=cba;
      try {
        while(!current.isDone) {
           current=current.step(ctx);
        }
      } catch {
         case ex:CallCCThrowable[_] if ex.aType <:< cba.aType => {
              current = ex.current.asInstanceOf[ComputationBounds[A]];
              inex=true;
         }
      }
      (current,inex); 
   }

  def seq[A:TypeTag](l:IndexedSeq[ComputationBounds[A]]) 
                         (implicit ctx:CallNesting):ComputationBounds[IndexedSeq[A]] =  
                                              seq1(l.companion[A](),l);
                              

  private case class LState[A](val lDone:IndexedSeq[A] = IndexedSeq(),
               val lInProcess:IndexedSeq[ComputationBounds[A]] = IndexedSeq(),
               val wasException: Boolean = false
                          );

  def seq1[A:TypeTag](l1:IndexedSeq[A],l2:IndexedSeq[ComputationBounds[A]])
                     (implicit ctx:CallNesting):ComputationBounds[IndexedSeq[A]] =
  {
    if (l2.isEmpty) {
      Done(l1);
    }else{
      ctx.withCall {
         (ctx:CallNesting) => implicit val ictx = ctx;
         var s = LState(l1,l2,false);
         while(!s.lInProcess.isEmpty) {
           s = s.lInProcess.map{
                 (x:ComputationBounds[A])=>reduce(x)
               }.foldRight(LState(s.lDone,IndexedSeq[ComputationBounds[A]]())) {
                 (x:(ComputationBounds[A],Boolean),s:LState[A]) =>
                 if (x._2 || s.wasException) {
                    LState(s.lDone, x._1 +: s.lInProcess, true)
                 } else if (x._1.isDone && s.lInProcess.isEmpty) {
                    LState(x._1.result.get+:s.lDone,s.lInProcess,s.wasException)
                 } else {
                    LState(s.lDone, x._1 +: s.lInProcess,s.wasException)
                 }
               }
           if (s.wasException) {
              throw new CallCCThrowable(Call{
                 (ctx:CallNesting) => implicit val ictx = ctx; 
                                      seq1(s.lDone,s.lInProcess)
              })
           }
         }
         Done(s.lDone);
      }
   }
  }

  def onProgress[A:TypeTag](ca:ComputationBounds[A])(action:CallNesting=>Unit)
                                                 (implicit ctx:CallNesting):
                                                              ComputationBounds[A]=
  {
    compose(ca,{ (x:A, nesting: CallNesting) => action(nesting); Done(x) });
  }

  def some[A](ca:ComputationBounds[A])(implicit ctx:CallNesting, aTag:TypeTag[A]): ComputationBounds[Option[A]] =
  {
    compose(ca,{ (x:A,ctx:CallNesting) => Done(Some(x)) });
  }

  final val MAX_NESTING=100;

}


