package ua.gradsoft.termware.freeterms;

import ua.gradsoft.termware._;

/**
 * signature for 'Functional' term in free algebra.
 * with fixed name and arity
 **/
trait FixedNameSignature 
{
  this:FunctionalTermSignature =>
   
  def fixedName:Option[Name]=Some(name);

  val name: Name;
}
