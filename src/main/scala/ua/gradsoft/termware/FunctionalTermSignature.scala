package ua.gradsoft.termware;

/**
 * signature for 'Functional' term in free algebra.
 **/
trait FunctionalTermSignature extends TermSignature
                                  with GeneralUtil
{

   /**
    * throw undefined.
    **/
   def createConstant(arg:Any): Term = { throwUOE; }

   /**
    * throw undefined.
    **/
   def createSpecial(args: Any*): Term = { throwUOE; }

   /**
   * return t.
   **/
   def toAnyRef(t:Term) = t;

  /**
   * return t
   **/
  def toAny(t:Term) = t;


}
