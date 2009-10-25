package ua.gradsoft.termware;

/**
 * API for access values
 */
trait TValue
{

  /**
   * is this is boolean ?
   */
  def isBoolean: Boolean = false;

  /**
   * get boolean value if one is boolean.
   */
  def getBoolean: Option[Boolean] = None;

  def isInt : Boolean = false;
  
  def getInt: Option[Int] = None;

  def isLong:  Boolean = false;

  def getLong: Option[Long] = None;

  def isString:  Boolean = false;

  def getString: Option[String] = None;

  def isException: Boolean = false;

  def getException: Option[Exception] = None;

}
