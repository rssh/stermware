import scala.annotation._;



class CallCCException[A](val current:ComputationBounds[A],
                         val ctx:CallContext) extends Exception;

object CallCC
{
  def apply[A](ctx:CallContext)(block:ComputationBounds[A]):Nothing
         = throw new CallCCException(block,ctx);
}

case class CallContext(val nesting:Int=0)
{

  def withCall:CallContext = CallContext(nesting+1);

  def checkStackOverflow: Boolean = {
      return (nesting >= ComputationBoundsHelper.MAX_NESTING);
  } 

}


object CallContext
{
  val empty = CallContext();
}

sealed trait ComputationBounds[A]
{
  def isDone: Boolean;
  def result: Option[A]
  def step(ctx:CallContext): ComputationBounds[A]
}

case class Done[A](val r: A) extends ComputationBounds[A]
{
  def isDone: Boolean = true;
  def result: Option[A] = Some(r);
  def step(ctx:CallContext) = this;
}

case class Call[A](
             thunk: (CallContext) => ComputationBounds[A]
           ) extends ComputationBounds[A]
{
  def isDone: Boolean = false;
  def result: Option[A] = None;
  def step(ctx:CallContext) = {
      thunk(ctx.withCall);
  }
}

case class ContCall[A,B](
             thunk: (CallContext) => ComputationBounds[A],
             cont: (A,CallContext) => ComputationBounds[B]
                        ) extends ComputationBounds[B]
{
  def isDone: Boolean = false;
  def result: Option[B] = None;
  def step(ctx:CallContext): ComputationBounds[B]={
        val s = thunk(ctx.withCall);
        if (s.isDone) {
            val a = s.result.get;
            cont(a,ctx.withCall);
        }else{
             ComputationBoundsHelper.createCont(s,cont,ctx.withCall);
        }
  }
}


object ComputationBoundsHelper
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

  def createCont[A,B](ca:ComputationBounds[A],
                      cont:(A,CallContext)=>ComputationBounds[B],
                      ctx:CallContext):
                                               ComputationBounds[B] = {
       var current = ca;
       var quit = false;
       var result: Option[ComputationBounds[B]] = None;
       while(!quit) {
         if (ctx.checkStackOverflow) { 
           CallCC(ctx)(Call{ (ctx:CallContext)=> 
                                      createCont(ca,cont,ctx.withCall) });
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
                                      createCont(ca,cont,ctx.withCall) });
             }
           }
         } 
       }
       result.get;
  }

  val MAX_NESTING=100;

}

trait Term
{

  def isInt: Boolean;
  def getInt: Int;

  def isFunctional: Boolean;
  def funName: String;
  def funArity: Int;
  def funArgs: Seq[Term];

  def unify (t:Term): Boolean;

  def onUnifyCont[T](t:Term)(cont:Boolean=>T) = {
     cont(unify(t));
  }

  import ComputationBoundsHelper._;

  def unifyCB2(t:Term, ctx:CallContext): ComputationBounds[Boolean] =
    { 
       Done(unify(t)); 
    }

  def fixUnifyCB2[T](t:Term): Boolean = 
          trampoline(Call{ (ctx:CallContext)=> unifyCB2(t,ctx) });

  def onUnifyCB2[T](t:Term, ctx: CallContext)
      (cont:(Boolean,CallContext)=>ComputationBounds[T]):ComputationBounds[T] = 
              {
               val f = unifyCB2(t, ctx.withCall);
               ComputationBoundsHelper.createCont(f, cont, ctx); 
              }


}


case class IntTerm(val v:Int) extends Term
{
  def isInt = true; 
  def getInt = v; 

  def isFunctional = false;
  def funName = v.toString;
  def funArity = 0;
  def funArgs = Seq[Term]();

  def unify(t:Term):Boolean = if (t.isInt) t.getInt==v else false;

}

case class FunTerm(val name:String, val args:Seq[Term]) extends Term
{

  def isInt = false; 
  def getInt = 0; 

  def isFunctional = true;
  def funName = name;
  def funArity = args.length;
  def funArgs = args;

  def unify(t:Term): Boolean = { 
   if (t.isFunctional) {
      if (t.funName == name) {
	 unifySequences(funArgs,t.funArgs) 
      } else {
	 false;
      }    
   } else {
     false;
   }
  }


  def unifySequences(x:Seq[Term],y:Seq[Term]): Boolean = 
                                       unifySequencesPrivate(x,y);

  @tailrec
  private def unifySequencesPrivate(x:Seq[Term],y:Seq[Term]): Boolean = {
     if (x.isEmpty) 
       y.isEmpty
      else {
       if (x.head.unify(y.head)) 
         unifySequencesPrivate(x.tail,y.tail)
       else
         false
      }
  }

  override def onUnifyCont[T](t:Term)(cont:Boolean=>T):T = {
     //System.err.println("onUnifyConf:"+this+", "+t);
     if (t.isFunctional) {
       if (t.funName == name) {
	 onUnifySeqCont(funArgs,t.funArgs)(cont)
       } else {
	 cont(false) 
       }
     } else {
       cont(false) 
     }
  }


  def onUnifySeqCont[T](x:Seq[Term],y:Seq[Term])(cont:Boolean=>T):T = { 
    if (x.isEmpty) cont (y.isEmpty) 
    else {
      x.head.onUnifyCont(y.head) {
	(r:Boolean) =>
	if (r) {
	  onUnifySeqCont(x.tail,y.tail)(cont)
	} else {
	  cont(false)
	}
      }
    }
  }


  import ComputationBoundsHelper._;

  override def unifyCB2(t:Term, ctx:CallContext):ComputationBounds[Boolean]=
  {
     if (ctx.checkStackOverflow) {
       CallCC(ctx)(Call{ (ctx:CallContext) => unifyCB2(t,ctx) });
     } else {
       if (t.isFunctional) {
         if (t.funName == name) {
            return unifySeqCB2(funArgs,t.funArgs,ctx.withCall);
         } else {
            Done(false);
         }
       } else {
          Done(false);
       }
     }
  }


  def unifySeqCB2(x:Seq[Term],y:Seq[Term],ctx:CallContext): 
                                             ComputationBounds[Boolean]=
  {
    if (x.isEmpty) {
       Done(y.isEmpty) 
    } else {
       if (ctx.checkStackOverflow) {
         CallCC(ctx)(Call{ (ctx) => unifySeqCB2(x,y,ctx) }); 
       } else {
         x.head.onUnifyCB2(y.head,ctx.withCall) {
          (r:Boolean, ctx:CallContext) => 
             if (!r) {
               Done(false)
             } else {
               if (x.isEmpty) {
                  Done(y.isEmpty);
               }else{
                  unifySeqCB2(x.tail,y.tail,ctx.withCall) 
               }
             }
         }
       }
    }
  }


}

object main
{
 val x = FunTerm("a",Seq[Term](IntTerm(1),IntTerm(2)));
 val y = FunTerm("a",Seq[Term](IntTerm(1),IntTerm(2)));

 def createHVTerm(nHorItems:Int,nVerItems:Int, incVal:Int):Term = {
    val  hseq = (0 to nHorItems) map { x:Int => IntTerm(x+incVal) }
    var retval = FunTerm("S0",hseq);
    for(i <- 0 to nVerItems) {
       retval = FunTerm("A"+i, hseq.map { (x:Term) => retval })
    } 
    retval;
 }
 
 def main(args:Array[String])={
   var result = x unify y;
   println("result is "+result);
   x.onUnifyCont(y) {
      (r:Boolean) => println("result is "+r);
   }
   val hv1 = createHVTerm(2,10000,1); 
   val hv2 = createHVTerm(2,10000,2); 
   println("created hv1, hv2");
   println("unification 1");
   try {
     result = hv1 unify hv2;
     println("result is "+result);
   } catch {
     case ex: StackOverflowError => println("Stack overflow");
   }
   println("unification 2");
   try {
     hv1.onUnifyCont(hv2) {
      (r:Boolean) => println("result is "+r);
     }
   } catch {
     case ex: StackOverflowError => println("Stack overflow");
   }
   println("unification 3");
   try {
     result = hv1 fixUnifyCB2 hv2 
     println("result is "+result);
   } catch {
     case ex: StackOverflowError => println("Stack overflow");
     ex.printStackTrace();
   }
   println("unification 3 with same");
   try {
     result = hv1 fixUnifyCB2 hv1 
     println("result is "+result);
   } catch {
     case ex: StackOverflowError => println("Stack overflow");
     ex.printStackTrace();
   }
 }


}


