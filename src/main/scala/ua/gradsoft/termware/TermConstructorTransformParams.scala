package ua.gradsoft.termware;

/**
 * let transform params.
 * Hold reference to parameters of transformation, which must
 * be performed at lazy val after initialization, to release reference
 * of this parameters after transformation will be finished.
 **/
class TermConstructorTransformParams[T <: Term](iniDoTransformation:Boolean,
                                     iniSubstParams: Option[Pair[PartialFunction[Term,Term],T]],
                                     neverCallMainConstructor: Null)    
{

  def this(doTransformation:Boolean) = this(doTransformation,None,null);
  def this(substParams: Pair[PartialFunction[Term,Term],T]) = this(false,Some(substParams),null);
  
  def isReleased = _released;
  def releaseRefs: Unit = {
    _released = true;
    _substParams = None;
  }
  
  def isDoTransformation:Boolean = _doTransformation;
  def substParams: Option[Pair[PartialFunction[Term,Term],T]] = {
    if (_released) {
       throw new IllegalStateException("attemt to get transform params after transformation");
    } else {
       _substParams;
    }
  }


  private[this] var _released: Boolean = false;
  private[this] var _doTransformation: Boolean = iniDoTransformation;
  private[this] var _substParams: Option[Pair[PartialFunction[Term,Term],T]] = iniSubstParams;
}

object TermConstructorTransformParams
{

  def apply[T <: Term](doTransformation:Boolean) = new TermConstructorTransformParams[T](doTransformation,None,null);
  def apply[T <: Term](substParams: Pair[PartialFunction[Term,Term],T]) = 
          new TermConstructorTransformParams[T](false,Some(substParams),null);

}

