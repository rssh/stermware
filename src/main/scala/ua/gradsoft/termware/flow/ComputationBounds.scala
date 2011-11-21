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

  // return manifest of A
  def aManifest: Manifest[_ <: A]

}

/**
 * when we have some result.
 **/
case class Done[A](val r: A)(implicit m:Manifest[A]) extends ComputationBounds[A]
{
  def isDone: Boolean = true;
  def result: Option[A] = Some(r);
  def step(ctx:CallContext) = this;
  def aManifest = m;
}

case class Call[A](
             thunk: (CallContext) => ComputationBounds[A]
           )(implicit m:Manifest[A]) extends ComputationBounds[A]
{
  def isDone: Boolean = false;
  def result: Option[A] = None;
  def step(ctx:CallContext) = {
      thunk(ctx);
  }
  def aManifest = m;
}

case class ContCall[A,B,XA <: A](
             thunk: (CallContext) => ComputationBounds[XA],
             cont: (XA,CallContext) => ComputationBounds[B]
             )
             (implicit ma:Manifest[A],
                       mb:Manifest[B],
                       mxa:Manifest[XA])
              extends ComputationBounds[B]
{
  def isDone: Boolean = false;
  def result: Option[B] = None;
  def step(ctx:CallContext): ComputationBounds[B]={
        val s = try {
                 thunk(ctx);
                } catch {
                  case ex: CallCCThrowable[_] if (ex.aManifest <:< ma) =>
                   {
                    implicit val ictx=ex.ctx;
                    throw new CallCCThrowable(Call{
                                           (ctx:CallContext) => step(ctx) });
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
  def aManifest = manifest[B];
}


