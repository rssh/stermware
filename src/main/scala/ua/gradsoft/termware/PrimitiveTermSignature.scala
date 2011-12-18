package ua.gradsoft.termware;

/**
 * Base signature for primitive types
 */
abstract class PrimitiveTermSignature(th:Theory) extends SimpleTermSignature
                                                     with GeneralUtil
{

  override def fixedName:Option[Name] = None;

  override def createTerm(name:Name, args: IndexedSeq[Term]) = throwUOE;
  override def createSpecial(args: Any*):Term = throwUOE;

  def from[T](x:T)(implicit mt: Manifest[T]):Option[Term] = fromAny(x);
  
  
  /**
   * name of class in type algebra.
   * Must be overriden in subclass
   **/
  def typeName: String;

  override def termType(t:Term):Term = termType;

  lazy val termType = theory.freeAtomSignature.createConstant(typeName);

  val theory=th;

}
