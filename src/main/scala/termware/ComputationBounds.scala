package termware;

import scala.reflect.runtime.universe._

/**
 * result of 'computation chunk' in trampoline.
 **/
sealed abstract class ComputationBounds[+A]
{

  // true when we have result.
  def isDone: Boolean;

  // resutl of computations.
  def result: Option[A]

  // receive next computation bounds if we not done.
  def step(ctx:CallNesting): ComputationBounds[A]

  def aTag: TypeTag[_ <: A]

  // type of computation bounds
  def aType: Type 

}

/**
 * when we have some result.
 **/
case class Done[A](val r: A)(implicit m:TypeTag[A]) extends ComputationBounds[A]
{
  def isDone: Boolean = true;
  def result: Option[A] = Some(r);
  def step(ctx:CallNesting) = this;
  def aTag = m;
  def aType = typeOf[A]
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
  def aTag = m;
  def aType = typeOf[A]
}

/**
 * Compose two computations
 **/
case class ContCall[A,B,XA <: A](
             thunk: (CallNesting) => ComputationBounds[XA],
             cont: (XA,CallNesting) => ComputationBounds[B]
             )
             (implicit ma:TypeTag[A],
                       mb:TypeTag[B],
                       mxa:TypeTag[XA])
              extends ComputationBounds[B]
{
  def isDone: Boolean = false;
  def result: Option[B] = None;
  def step(ctx:CallNesting): ComputationBounds[B]={
        val s = try {
                 thunk(ctx);
                } catch {
                  case ex: CallCCThrowable[_] if (ex.aType <:< typeOf[A]) =>
                   {
                    throw new CallCCThrowable(Call{ ctx => step(ctx) });
                   }
                }
          if (s.isDone) {
            val a = s.result.get;
            cont(a,ctx);
          }else{
            implicit val ictx = ctx;
            CallCC.compose(s,cont);
          }
  }
  def aTag = typeTag[B];
  def aType = typeOf[B];
}


