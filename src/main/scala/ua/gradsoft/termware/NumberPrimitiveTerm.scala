package ua.gradsoft.termware;

import java.io.PrintWriter;

/**
 * base class for number constants
 **/
abstract class NumberPrimitiveTerm[T](v:T, s:TermSignature) 
                                                   extends PrimitiveTerm(s)
{

  def isBoolean: Boolean = false;

  def getBoolean: Boolean = throwUOE;

  override def isNumber: Boolean = true;

  def termClassIndex: Int = TermClassIndex.NUMBER;

  val value = v;

  override def print(out:PrintWriter):Unit = { out.print(value); }

}

