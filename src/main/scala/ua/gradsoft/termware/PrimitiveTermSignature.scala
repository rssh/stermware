package ua.gradsoft.termware;

/**
 * Base signature for primitive types
 */
trait PrimitiveTermSignature extends TermSignature
{

  override def fixedName = None;

  override def fixedArity = Some(0);

  override def isConcrete = true;

  override def nameByIndex = None;
  override def indexByName = None;

  override def createTerm(name:Name, args: Term*) = None;
  override def createSpecial(args: Any*) = None;

}
