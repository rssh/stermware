package termware;

import scala.reflect.runtime.universe._
import scala.concurrent._


/**
 * result of 'computation chunk' in trampoline.
 **/
sealed abstract class ComputationBounds[+A]
{

  thisComputationBounds =>

  import ComputationBounds._

  /**
   * true when we have result.
   */
  def isDone: Boolean;

  /**
   * resutl of computations.
   */
  def result: Option[A]

  // receive next computation bounds if we not done.
  def step(tid:TrampolineId, depth:Int): ComputationBounds[A]


  def toFuture(implicit executor:ExecutionContext): Future[A] = 
         future{ trampoline(this); }
                            
  // Monadic syntax


  def get: A = trampoline(this);

  def map[B](f: A => B): ComputationBounds[B] = flatMap((x:A) => Done(f(x)))


  def flatMap[B](f: A => ComputationBounds[B]) = 
            compose(this, (a:A)=>f(a))

  /**
   *  monadic interface where client can see trampolineId and depth.
   **/
  class InProcess {
     
      def map[B](f: A => (TrampolineId,Int) => B): ComputationBounds[B] =
                        flatMap(x => (tid,depth) => Done(f(x)(tid,depth)))

      def flatMap[B](f: A => (TrampolineId,Int) => ComputationBounds[B]) = 
            compose(thisComputationBounds, (tid,depth,a:A)=>f(a)(tid,depth))


  }

  def inProcess = new InProcess

}



/**
 * when we have some result.
 **/
case class Done[A](val r: A) extends ComputationBounds[A]
{
  def isDone: Boolean = true;
  def result: Option[A] = Some(r);
  def step(tid:TrampolineId, depth:Int) = this;
}

/**
 * when we need to call net thunk
 **/
case class Call[A](
             thunk: (TrampolineId, Int) => ComputationBounds[A]
           ) extends ComputationBounds[A]
{
  def isDone: Boolean = false
  def result: Option[A] = None
  def step(tId:TrampolineId,depth:Int) = 
      thunk(tId,depth)
}

object Call
{

   def nested[A](tid:TrampolineId, nesting:Int)(block : ((TrampolineId,Int) => ComputationBounds[A])) =
     if (nesting > ComputationBounds.MAX_NESTING) {
         throw new CallCCThrowable(tid,Call{block}) 
     } else {
         block(tid,nesting+1);
     }

}



case class Compose[A,B] private[termware](source:ComputationBounds[A],
                                conts: ComputationBounds.Cont[A,B]
                               ) extends ComputationBounds[B]
{
  import ComputationBounds._

  def isDone: Boolean = false;

  def result: Option[B] = None;

  def step(tid:TrampolineId, depth:Int) = {
       source match {
         case Done(a) =>
            conts match {
               case ContOne(f,n) =>
                 try {
                   f(tid,depth,a)
                 } catch {
                   case ex:CallCCThrowable[B] if ex.tid == tid =>
                        ex.current       
                 }
               case  ContCons(f,g) =>
                 f match {
                   case ContOne(ff,fdepth) =>
                      var s = try {
                                ff(tid,depth,a)
                              } catch {
                                case ex:CallCCThrowable[_] if ex.tid == tid =>
                                   ex.current       
                              }
                      s match {
                       case Done(_) => Compose(s,g)
                       case Call(_) => Compose(s,g)
                       case Compose(s,f1) => Compose(s,append(f1,g))
                     }
                   case ContCons(f1,f2) =>
                     Compose(source,ContCons(f1,ContCons(f2,g)))  
                 }
            }
         case Call(_) =>
            val r = source.step(tid,depth);
            r match {
              case Done(x) => Compose(r,conts)
              case Call(_) => Compose(r,conts)
              case Compose(s,f1) => Compose(s,append(f1,conts))
            }
         case Compose(s,f1) =>  Compose(s,append(f1,conts))
       }     
  }

}


object ComputationBounds
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

  sealed trait Cont[A,+B]
  {
   type From = A;
   type To = B;
  }

  case class ContOne[A,B](f: (TrampolineId,Int,A)=> ComputationBounds[B], n: Int = 0) extends Cont[A,B]

  case class ContCons[A,B,C](val x:Cont[A,B],
                             val y:Cont[B,C]) extends Cont[A,C]

  type ~>[A,B] = Cont[A,B]

  def append[A,B,C](f: A ~> B, g: B ~> C): A ~> C =
  {
    //System.err.println("append ("+f+","+g+")");
    f match {
      case ContOne(ff,nf) => 
        g match {
           case ContOne(fg,ng) if (nf + ng + 1 < MAX_NESTING) =>
                                ContOne({ (tid,depth,a) =>
                                           if (depth >= MAX_NESTING - 1) {
                                               Compose(Done(a),ContCons(f,g))
                                           } else {
                                               val af = try {
                                                            ff(tid,depth+1,a)
                                                        } catch {
                                                         case ex: CallCCThrowable[B] if (ex.tid==tid) =>
                                                              ex.current
                                                        }
                                               if (af.isDone) 
                                                  fg(tid,depth+1,af.result.get)
                                               else
                                                  Compose(af,g)
                                           }
                                        },
                                        nf + ng + 1)
           case ContCons(g1,g2) =>
                  g1 match {
                    case ContOne(fg1,ng1) if (nf+ng1+1 < MAX_NESTING) =>
                            ContCons(append(f,g1),g2)
                    case _ => ContCons(f,g)
                  }
           case _ => ContCons(f,g)
        }
      case ContCons(f1,f2) => ContCons(f1,append(f2,g))
    }
  }


  @inline
  def compose[A,B](ca:ComputationBounds[A],
             cont:(TrampolineId,Int,A)=>ComputationBounds[B]): ComputationBounds[B] =
    ca match {
      case Compose(x,xcont) => Compose(x,append(xcont,ContOne(cont)))
      case _ => Compose(ca,ContOne(cont))         
    }


  @inline
  def compose[A,B](ca:ComputationBounds[A], cont:A=>ComputationBounds[B]): ComputationBounds[B] =
    compose(ca,{ (tid:TrampolineId, nesting:Int, x:A) => cont(x)});




  final val MAX_NESTING=500;


}


