package ua.gradsoft.termware;


/**
 * base class for number constants
 **/
abstract class NumberPrimitiveTerm[T](v:T, s:TermSignature) 
                                                   extends PrimitiveTerm(s)
{
  override def isNumber: Boolean = true;

  def termClassIndex: Int = TermClassIndex.NUMBER;

  val value = v;

}

