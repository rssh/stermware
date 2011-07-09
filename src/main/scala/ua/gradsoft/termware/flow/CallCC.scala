package ua.gradsoft.termware.flow;



object CallCC
{

  def trampoline[A](cb:ComputationBounds[A]):A = {
       var quit = false;
       var result: Option[A] = None;
       var current = cb;
       while(!quit) {
         if (current.isDone) {
             result=current.result;
             quit=true;
         } else {
             try {
               current = current.step(CallContext.empty);
             } catch {
                case ex: CallCCException[A] => current = ex.current;
             }
         }
       }
       return result.get;
  }

  def compose[A,B](ca:ComputationBounds[A],
                   cont:(A,CallContext)=>ComputationBounds[B])
                   (implicit ctx:CallContext):
                                               ComputationBounds[B] = 
       ctx.withCall {
         (ctx:CallContext)=> implicit val ictx = ctx; 
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
             case ex:CallCCException[A] => current=ex.current;
             if (current.isDone) {
                val sa = cont(current.result.get,ctx);
                throw new CallCCException(sa)(ex.ctx);
             } else {
                throw new CallCCException(Call{ (ctx:CallContext)=> 
                                          compose(ca,cont)(ctx) });
             }
           }
         } 
         result.get;
       }

  @inline
  def compose[A,B](ca:ComputationBounds[A],
                   cont:A=>ComputationBounds[B])
                   (implicit ctx:CallContext):
                                               ComputationBounds[B] = 
    compose(ca,{ (x:A,ctx:CallContext) => cont(x) })(ctx);


  def pair[A,B](ca:ComputationBounds[A],cb:ComputationBounds[B])
                                      (implicit ctx:CallContext):
                                           ComputationBounds[(A,B)] =
     ctx.withCall {
       (ctx:CallContext)=> implicit val ictx = ctx; 
       var cca = ca; 
       var ccb = cb; 
       var quit=false;
       while(!quit) {
          if (!cca.isDone) {
            try {
              cca=cca.step(ctx)
            } catch {
              case ex: CallCCException[A] => {
                cca = ex.current;
                throw new CallCCException(
                   Call{ (ctx:CallContext)=> pair(cca,ccb)(ctx) })(ex.ctx);
              }
            }
          } else if (ccb.isDone) {
            quit = true;
          }
          if (!ccb.isDone) {
            try {
              ccb=ccb.step(ctx)
            } catch {
              case ex: CallCCException[B] => {
                ccb = ex.current;
                throw new CallCCException(
                    Call{ (ctx:CallContext)=> pair(cca,ccb)(ctx) })(ex.ctx);
              }
            }
          }
       }
       Done((cca.result.get,ccb.result.get)); 
   }

   def tuple[A,B](ca:ComputationBounds[A],cb:ComputationBounds[B])
                                     (implicit ctx:CallContext) = pair(ca,cb);
              


   def tuple[A,B,C](ca:ComputationBounds[A],cb:ComputationBounds[B], 
                    cc:ComputationBounds[C])
                    (implicit ctx:CallContext): ComputationBounds[(A,B,C)] =
                compose(pair(ca,pair(cb,cc)),
                        { (x:(A,(B,C))) => Done((x._1, x._2._1, x._2._2)) }
                       );
                         

   @inline
   def reduce[A](cba: ComputationBounds[A])(implicit ctx:CallContext)
                                         :(ComputationBounds[A],Boolean) = {
      var inex = false;
      var current=cba;
      try {
        while(!current.isDone) {
           current=current.step(ctx);
        }
      } catch {
         case ex:CallCCException[A] => {
              current = ex.current;
              inex=true;
         }
      }
      (current,inex); 
   }

  def seq[A](l:IndexedSeq[ComputationBounds[A]]) 
            (implicit ctx:CallContext):ComputationBounds[IndexedSeq[A]] =  
                                              seq1(l.companion[A](),l)(ctx);
                              

  private case class LState[A](val lDone:IndexedSeq[A] = IndexedSeq(),
               val lInProcess:IndexedSeq[ComputationBounds[A]] = IndexedSeq(),
               val wasException: Boolean = false
                          );

  def seq1[A](l1:IndexedSeq[A],l2:IndexedSeq[ComputationBounds[A]])
              (implicit ctx:CallContext):ComputationBounds[IndexedSeq[A]] =
  {
    if (l2.isEmpty) {
      Done(l1);
    }else{
      ctx.withCall {
         (ctx:CallContext) => implicit val ictx = ctx;
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
              throw new CallCCException(Call{
                 (ctx:CallContext) => seq1(s.lDone,s.lInProcess)(ctx)
              })
           }
         }
         Done(s.lDone);
      }
   }
  }

  def onProgress[A](ca:ComputationBounds[A])(action:CallContext=>Unit)
                                                 (implicit ctx:CallContext):
                                           ComputationBounds[A]=
  {
    compose(ca,{ (x:A,ctx:CallContext) => action(ctx); Done(x) })(ctx);
  }

  def some[A](ca:ComputationBounds[A])(implicit ctx:CallContext): ComputationBounds[Option[A]] =
  {
    compose(ca,{ (x:A,ctx:CallContext) => Done(Some(x)) })(ctx);
  }

  final val MAX_NESTING=100;

}


