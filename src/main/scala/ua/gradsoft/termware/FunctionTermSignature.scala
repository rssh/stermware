package ua.gradsoft.termware;

/**
 * signature for 'Functional' term in free algebra.
 **/
trait FunctionalTermSignature extends TermSignature
                                  with GeneralUtil
{

   def createConstant(arg:Any): Term = { throwUOE; }

   def createSpecial(args: Any*): Term = { throwUOE; }
}
