package termware;

import scala.reflect.runtime.universe._
import scala.concurrent._

/**
 * result of 'computation chunk' in trampoline.
 **/
sealed abstract class ComputationBounds[+A:TypeTag]
{

  // true when we have result.
  def isDone: Boolean;

  // resutl of computations.
  def result: Option[A]

  // receive next computation bounds if we not done.
  def step(nesting:CallNesting): ComputationBounds[A]

  def aTag: TypeTag[_ <: A] = typeTag[A]

  // type of computation bounds
  def aType: Type = typeOf[A]

  def andThen[B:TypeTag](f: A => B): ComputationBounds[B] = Call{ nesting => CallCC.compose(this, (a:A) => Done(f(a)))(nesting) }

  def toFuture(implicit executor:ExecutionContext): Future[A] = future{ CallCC.trampoline(this); }
                            

}

/**
 * when we have some result.
 **/
case class Done[A:TypeTag](val r: A) extends ComputationBounds[A]
{
  def isDone: Boolean = true;
  def result: Option[A] = Some(r);
  def step(ctx:CallNesting) = this;
}

/**
 * when we need to call net thunk
 **/
case class Call[A](
             thunk: (CallNesting) => ComputationBounds[A]
           )(implicit m:TypeTag[A]) extends ComputationBounds[A]
{
  def isDone: Boolean = false;
  def result: Option[A] = None;
  def step(ctx:CallNesting) = {
      thunk(ctx);
  }
}



