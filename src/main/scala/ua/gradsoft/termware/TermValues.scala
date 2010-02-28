package ua.gradsoft.termware;

import java.lang.Number;

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
  def isByte:  Boolean;

  /**
   * get byte value if one is byte.
   */
  def getByte: Option[Byte];

  /**
   *  is this is short ?
   **/
  def isShort:  Boolean;

  /**
   *  get short value.
   **/
  def getShort:  Option[Short];

  /**
   *  is this is integer ?
   **/
  def isInt:  Boolean;

  /**
   * get int value
   **/
  def getInt: Option[Int];

  /**
   * is this is long ?
   **/
  def isLong:  Boolean;

  /**
   * get long is one is long.
   **/
  def getLong: Option[Long];

  /**
   * if this is big integer
   **/
  def isBigInt: Boolean;

  /**
   * get BigInteger is one is big integer
   **/
  def getBigInt: Option[BigInt];

  /**
   * it this term can be represented as big decimal ?
   **/
  def isBigDecimal: Boolean;

  def getBigDecimal: Option[BigDecimal];

  def isFloat: Boolean;

  def getFloat: Option[Float];

  def isDouble: Boolean;

  def getDouble: Option[Double];

  /**
   * if this is number type ?
   **/
  def isNumber: Boolean ;

  def getNumber: Option[Number];

  def getNumberKind: Option[Int];

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
