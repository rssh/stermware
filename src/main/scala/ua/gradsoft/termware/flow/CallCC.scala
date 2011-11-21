package ua.gradsoft.termware.flow;

import scala.reflect._;


object CallCC
{

  def trampoline[A](cb:ComputationBounds[A])(implicit m:Manifest[A]):A = {
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
                case ex : CallCCThrowable[_] if (ex.aManifest <:< m)
                    => current = ex.current.asInstanceOf[ComputationBounds[A]]
             }
         }
       }
       return result.get;
  }

  def compose[A,B](ca:ComputationBounds[A],
                  cont:(A,CallContext)=>ComputationBounds[B])
                (implicit ctx:CallContext,ma:Manifest[A],mb:Manifest[B]):
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
             case ex:CallCCThrowable[_] if (ex.aManifest <:< ca.aManifest)
                  => current=ex.current.asInstanceOf[ComputationBounds[A]];
             if (current.isDone) {
                val sa = cont(current.result.get,ctx);
                throw new CallCCThrowable(sa)(ex.ctx,manifest[B]);
             } else {
                throw new CallCCThrowable(Call{ (ctx:CallContext)=> 
                                          implicit val ictx=ctx; 
                                          compose(ca,cont); });
             }
           }
         } 
         result.get;
       }

  def fcompose[A:Manifest,B:Manifest](ca:ComputationBounds[A],
                    cont:(A,CallContext)=>ComputationBounds[B]):
                                                 ComputationBounds[B]=
    Call { (ctx:CallContext) => implicit val ictx = ctx; compose(ca,cont); }

  @inline
  def compose[A,B](ca:ComputationBounds[A],
                   cont:A=>ComputationBounds[B])
                   (implicit ctx:CallContext, ma: Manifest[A], mb: Manifest[B]):
                                               ComputationBounds[B] = 
    compose(ca,{ (x:A,ctx:CallContext) => cont(x) });


  def pair[A,B](ca:ComputationBounds[A],cb:ComputationBounds[B])
               (implicit ctx:CallContext, ma: Manifest[A], mb: Manifest[B]):
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
              case ex: CallCCThrowable[_] if ex.aManifest <:< manifest[A] => {
                cca = ex.current.asInstanceOf[ComputationBounds[A]];
                implicit val ictx = ex.ctx;
                throw new CallCCThrowable(
                   Call{ (ctx:CallContext)=> implicit val ictx = ctx; pair(cca,ccb); });
              }
            }
          } else if (ccb.isDone) {
            quit = true;
          }
          if (!ccb.isDone) {
            try {
              ccb=ccb.step(ctx)
            } catch {
              case ex: CallCCThrowable[_] if ex.aManifest <:< manifest[B] => {
                ccb = ex.current.asInstanceOf[ComputationBounds[B]];
                implicit val ictx = ex.ctx;
                throw new CallCCThrowable(
                    Call{ (ctx:CallContext)=> implicit val ictx=ctx; pair(cca,ccb); });
              }
            }
          }
       }
       Done((cca.result.get,ccb.result.get)); 
   }

   def tuple[A,B](ca:ComputationBounds[A],cb:ComputationBounds[B])
                 (implicit ctx:CallContext, ma:Manifest[A], mb: Manifest[B]) = pair(ca,cb);
              


   def tuple[A,B,C](ca:ComputationBounds[A],cb:ComputationBounds[B], 
                    cc:ComputationBounds[C])
                    (implicit ctx:CallContext, 
                     ma:Manifest[A], mb:Manifest[B], mc:Manifest[C]): ComputationBounds[(A,B,C)] =
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
         case ex:CallCCThrowable[_] if ex.aManifest <:< cba.aManifest => {
              current = ex.current.asInstanceOf[ComputationBounds[A]];
              inex=true;
         }
      }
      (current,inex); 
   }

  def seq[A](l:IndexedSeq[ComputationBounds[A]]) 
            (implicit ctx:CallContext, ma: Manifest[A]):ComputationBounds[IndexedSeq[A]] =  
                                              seq1(l.companion[A](),l);
                              

  private case class LState[A](val lDone:IndexedSeq[A] = IndexedSeq(),
               val lInProcess:IndexedSeq[ComputationBounds[A]] = IndexedSeq(),
               val wasException: Boolean = false
                          );

  def seq1[A](l1:IndexedSeq[A],l2:IndexedSeq[ComputationBounds[A]])
              (implicit ctx:CallContext,ma:Manifest[A]):ComputationBounds[IndexedSeq[A]] =
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
              throw new CallCCThrowable(Call{
                 (ctx:CallContext) => implicit val ictx = ctx; 
                                      seq1(s.lDone,s.lInProcess)
              })
           }
         }
         Done(s.lDone);
      }
   }
  }

  def onProgress[A](ca:ComputationBounds[A])(action:CallContext=>Unit)
                                                 (implicit ctx:CallContext,
                                                           ma:Manifest[A]):
                                           ComputationBounds[A]=
  {
    compose(ca,{ (x:A,ctx:CallContext) => action(ctx); Done(x) });
  }

  def some[A](ca:ComputationBounds[A])(implicit ctx:CallContext, ma:Manifest[A]): ComputationBounds[Option[A]] =
  {
    compose(ca,{ (x:A,ctx:CallContext) => implicit val ictx=ctx; Done(Some(x)) });
  }

  final val MAX_NESTING=100;

}


