package ua.gradsoft.termware;

import java.io.PrintWriter;

/**
 * base class for number constants
 **/
abstract class NumberPrimitiveTerm[T](val v:T, s:TermSignature)(implicit mt: Manifest[T]) 
                                                   extends PrimitiveTerm(s)
{
	
  /**
   * get optional value of given type.	
   */
  def optValue[U](implicit mu:Manifest[U]):Option[U] =
  {
  	if (mu <:< mt) Some(v.asInstanceOf[U]) else None ;  
  }
	
  /**
   * false
   */
  def isBoolean: Boolean = false;

  def getBoolean: Boolean = throwUOE;

  override def isNumber: Boolean = true;

  def termClassIndex: Int = TermClassIndex.NUMBER;

  override def print(out:PrintWriter):Unit = { out.print(value); }

}

