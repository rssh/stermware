package ua.gradsoft.termware;


/**
 * Term, which can be represent in scala/java only as reference.
 **/
trait RefTranslatedTermSignature
{
  this: TermSignature =>

  @inline
  final def toAny(t:Term) = toAnyRef(t)

  final def fromAny(x:Any) =
    x match {
      case r: AnyRef => fromAnyRef(r)
      case _ => None
    }
}
