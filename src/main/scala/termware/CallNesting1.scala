package termware;

import scala.reflect.runtime.universe._

/**
 * CallNesting
 *  passed in each call, used for tracking recursion depth in trampoline
 *  implementation.
 **/
case class CallNesting1(val nesting: Int=0)  extends AnyVal
{

  /**
   * generate new context with increased level of nesting.
   **/
  def next:CallNesting1 = new CallNesting1(nesting+1);

  /**
   * call block in next level of nesting
   **/
/*
  @inline
  def withCall[Y:TypeTag](block:CallNesting1=>ComputationBounds[Y]):
                                             ComputationBounds[Y] = 
        { 
          if (nesting > CallCC.MAX_NESTING) {
            throw new CallCCThrowable[Y](
                         Call{ ctx:CallNesting1 => block(ctx) });
          } else {
            block(this.next); 
          }
        }
*/

  /**
   * true, when level of nesting is require to thrw new exception.
   **/
  @inline
  def stackBehindLimit:Boolean = (nesting > CallCC.MAX_NESTING);

}

object CallNesting1
{
  /**
   * initial empty context.
   **/
  val empty = new CallNesting1();
}
