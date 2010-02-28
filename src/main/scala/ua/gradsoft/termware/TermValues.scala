package ua.gradsoft.termware;


/**
 * API for access values, wrapped in terms.
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

  /**
   * is this is byte ?
   **/
  def isByte:  Boolean = false;

  /**
   * get byte value if one is byte.
   */
  def getByte: Option[Byte] = None;

  /**
   *  is this is short ?
   **/
  def isShort:  Boolean = false;

  /**
   *  get short value.
   **/
  def getShort:  Option[Short] = None;

  /**
   *  is this is integer ?
   **/
  def isInt:  Boolean = false;

  /**
   * get int value
   **/
  def getInt: Option[Int] = None;

  /**
   * is this is long ?
   **/
  def isLong:  Boolean = false;

  /**
   * get long is one is long.
   **/
  def getLong: Option[Long] = None;

  def isBigInt: Boolean = false;

  /**
   * get BigInteger is one is big integer
   **/
  def getBigInt: Option[BigInt] = None;

  def isBigDecimal: Boolean = false;

  def getBigDecimal: Option[BigDecimal] = None;

  def isFloat: Boolean = false;

  def getFloat: Option[Float] = None;

  def isDouble: Boolean = false;

  def getDouble: Option[Double] = None;

  /**
   * is this is number of one of integer types
   * (byte, short, int, long )
   **/
  def isIntegerNumber: Boolean = false;

  def isNumber: Boolean = false;

  def getNumber: Option[Number] = None;

  def isChar:  Boolean = false;

  def getChar: Option[Char] = None;

  def isString:  Boolean = false;

  def getString: Option[String] = None;

  /**
   * is this a special exception object ?
   **/
  def isException: Boolean = false;

  def getException: Option[Exception] = None;

  /**
   * is this is reference object ?
   * (i. e. derived from scala AnyRef or Java Object)
   **/
  def isRef: Boolean = false;

  def getRef: Option[AnyRef] = None;

}
