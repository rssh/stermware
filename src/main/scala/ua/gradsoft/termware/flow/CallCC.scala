package ua.gradsoft.termware.flow;



object CallCC
{
  def apply[A](ctx:CallContext)(block:ComputationBounds[A]):Nothing
         = throw new CallCCException(block,ctx);

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
                      cont:(A,CallContext)=>ComputationBounds[B],
                      ctx:CallContext):
                                               ComputationBounds[B] = {
       var current = ca;
       var quit = false;
       var result: Option[ComputationBounds[B]] = None;
       while(!quit) {
         if (ctx.stackBehindLimit) { 
           CallCC(ctx)(Call{ (ctx:CallContext)=> 
                                      compose(ca,cont,ctx.withCall) });
         }else{
           try {
             current=current.step(ctx.withCall);  
             if (current.isDone) {
               result=Some(cont(current.result.get,ctx.withCall));
               quit=true;
             }
           }catch{
             case ex:CallCCException[A] => current=ex.current;
             if (current.isDone) {
                val sa = cont(current.result.get,ctx.withCall);
                CallCC(ex.ctx)(sa);
             } else {
                CallCC(ctx)(Call{ (ctx:CallContext)=> 
                                      compose(ca,cont,ctx.withCall) });
             }
           }
         } 
       }
       result.get;
  }

  val MAX_NESTING=100;

}


