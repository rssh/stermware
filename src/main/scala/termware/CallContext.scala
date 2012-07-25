package termware;

import scala.reflect.runtime.universe._

/**
 * CallContext
 *  passed in each call, used for tracking recursion depth in trampoline
 *  implementation.
 **/
case class CallContext(val nesting: Int=0) extends AnyVal
{

  /**
   * generate new context with increased level of nesting.
   **/
  def next:CallContext = CallContext(nesting+1);

  /**
   * call block in next level of nesting
   **/
  @inline
  def withCall[Y:TypeTag](block:CallContext=>ComputationBounds[Y]):
                                             ComputationBounds[Y] = 
        { 
          if (nesting > CallCC.MAX_NESTING) {
            throw new CallCCThrowable[Y](
                         Call{ (ctx:CallContext) => ctx.withCall(block) });
          } else {
            block(this.next); 
          }
        }

  /**
   * true, when level of nesting is require to thrw new exception.
   **/
  @inline
  def stackBehindLimit:Boolean = (nesting > CallCC.MAX_NESTING);

}

object CallContext
{
  /**
   * initial empty context.
   **/
  val empty = CallContext();
}
