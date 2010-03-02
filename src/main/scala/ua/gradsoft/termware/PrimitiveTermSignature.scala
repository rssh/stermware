package ua.gradsoft.termware;

/**
 * Base signature for primitive types
 */
abstract class PrimitiveTermSignature(th:Theory) extends TermSignature
{

  override def fixedName = None;

  override def fixedArity = Some(0);

  override def nameByIndex = None;
  override def indexByName = None;

  override def createTerm(name:Name, args: RandomAccessSeq[Term]) = None;
  override def createSpecial(args: Any*) = None;

  /**
   * name of class in type algebra.
   * Must be overriden in subclass
   **/
  def typeName: String;

  override def getType(t:Term):Term = termType;
  lazy val termType = th.freeAtomSignature.createConstant(typeName).get;

  val theory: Theory = th;
}
