package ua.gradsoft.termware.freeterms;

import ua.gradsoft.termware._;

/**
 * signature for 'Functional' term in free algebra.
 * with fixed name and arity
 **/
trait FixedNameAndAritySignature extends FixedNameSignature
{
  this: FunctionalTermSignature =>

  def fixedArity:Option[Int]=Some(arity);

  val arity: Int;
}
