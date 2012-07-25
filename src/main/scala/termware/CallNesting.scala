package termware;

import scala.reflect.runtime.universe._

/**
 * CallNesting
 *  passed in each call, used for tracking recursion depth in trampoline
 *  implementation.
 **/
case class CallNesting(val nesting: Int=0) extends AnyVal
{

  /**
   * generate new context with increased level of nesting.
   **/
  def next:CallNesting = CallNesting(nesting+1);

  /**
   * call block in next level of nesting
   **/
  @inline
  def withCall[Y:TypeTag](block:CallNesting=>ComputationBounds[Y]):
                                             ComputationBounds[Y] = 
        { 
          if (nesting > CallCC.MAX_NESTING) {
            throw new CallCCThrowable[Y](
                         Call{ ctx:CallNesting => ctx.withCall(block) });
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

object CallNesting
{
  /**
   * initial empty context.
   **/
  val empty = CallNesting();
}
