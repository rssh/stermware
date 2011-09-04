package ua.gradsoft.termware;

trait NonBooleanTerm extends Term
{

  def isBoolean: Boolean = true;

  def getBoolean: Boolean = throwUOE;

}
