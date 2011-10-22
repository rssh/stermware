package ua.gradsoft.termware.flow;

/**
 * result of 'computation chunk' in trampoline.
 **/
sealed trait ComputationBounds[+A]
{

  // true when we have result.
  def isDone: Boolean;

  // resutl of computations.
  def result: Option[A]

  // receive next computation bounds if we not done.
  def step(ctx:CallContext): ComputationBounds[A]
}

/**
 * when we have some result.
 **/
case class Done[+A](val r: A) extends ComputationBounds[A]
{
  def isDone: Boolean = true;
  def result: Option[A] = Some(r);
  def step(ctx:CallContext) = this;
}

case class Call[+A](
             thunk: (CallContext) => ComputationBounds[A]
           ) extends ComputationBounds[A]
{
  def isDone: Boolean = false;
  def result: Option[A] = None;
  def step(ctx:CallContext) = {
      thunk(ctx);
  }
}

case class ContCall[+A,+B,XA <: A](
             thunk: (CallContext) => ComputationBounds[XA],
             cont: (XA,CallContext) => ComputationBounds[B]
                        ) extends ComputationBounds[B]
{
  def isDone: Boolean = false;
  def result: Option[B] = None;
  def step(ctx:CallContext): ComputationBounds[B]={
        val s = try {
                 thunk(ctx);
                } catch {
                  case ex: CallCCThrowable[A] =>
                        throw new CallCCThrowable(Call{
                                (ctx:CallContext) => step(ctx) })(ex.ctx);
                }
          if (s.isDone) {
            val a = s.result.get;
            cont(a,ctx);
          }else{
            CallCC.compose(s,cont)(ctx);
          }
  }
}


