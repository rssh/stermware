package ua.gradsoft.termware.flow;

/**
 * CallContext
 *  passed in each call, used for tracking recursion depth in trampoline
 *  implementation.
 **/
case class CallContext(val nesting: Int=0) 
{

  /**
   * generate new context with increased level of nesting.
   **/
  def withCall:CallContext = CallContext(nesting+1);

  def stackBehindLimit: Boolean = { nesting >= CallCC.MAX_NESTING; }

}

object CallContext
{
  /**
   * initial empty context.
   **/
  val empty = CallContext();
}
