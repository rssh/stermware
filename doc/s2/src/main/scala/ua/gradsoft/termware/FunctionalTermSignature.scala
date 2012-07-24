package ua.gradsoft.termware;

/**
 * signature for 'Functional' term in free algebra.
 **/
trait FunctionalTermSignature extends TermSignature
                                  with GeneralUtil
                                  with RefTranslatedTermSignature
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
   * check that t is term.
   **/
  def fromAnyRef(x:AnyRef) =
   x match {
      case  t: Term => Some(t)
      case _ => None
   }
  
  //def to[T](implicit mt: Manifest[T]) = None;
  

}
