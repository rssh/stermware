package ua.gradsoft.termware;

/**
 * signature for 'Functional' term in free algebra.
 **/
trait FunctionalTermSignature extends TermSignature
{

   def createConstant(arg:Any): Option[Term] = None;

   def createSpecial(args: Any*): Option[Term] = None;

}
