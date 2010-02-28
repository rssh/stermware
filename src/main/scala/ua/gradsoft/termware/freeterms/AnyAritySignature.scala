package ua.gradsoft.termware.freeterms;

import ua.gradsoft.termware._;

/**
 * signature for 'Functional' term in free algebra.
 * with fixed name and arity
 **/
trait AnyAritySignature
{
  this: FunctionalTermSignature => 

  def fixedArity:Option[Int] = None;

  def indexByName:Option[Name=>Option[Int]] = None;
  def nameByIndex:Option[Int=>Option[Name]] = None;

}
