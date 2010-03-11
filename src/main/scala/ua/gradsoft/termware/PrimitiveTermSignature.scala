package ua.gradsoft.termware;

/**
 * Base signature for primitive types
 */
abstract class PrimitiveTermSignature(th:Theory) extends TermSignature
                                                 with GeneralUtil
{

  override def fixedName:Option[Name] = None;

  override def fixedArity = Some(0);

  override def nameByIndex = None;
  override def indexByName = None;

  override def createTerm(name:Name, args: RandomAccessSeq[Term]) = throwUOE;
  override def createSpecial(args: Any*) = throwUOE;

  /**
   * name of class in type algebra.
   * Must be overriden in subclass
   **/
  def typeName: String;

  override def getType(t:Term):Term = termType;
  lazy val termType = th.freeAtomSignature.createConstant(typeName);

  val theory: Theory = th;
}
