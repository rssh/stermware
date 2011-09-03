package ua.gradsoft.termware;

import java.lang.Number;

import ua.gradsoft.termware.flow._;

/**
 * API for access values, wrapped in terms.
 */
trait CbTValue extends GeneralUtil
{

  /**
   * is this is boolean ?
   */
  def isBoolean: ComputationBounds[Boolean] ;

  /**
   * get boolean value if one is boolean.
   */
  def getBoolean: ComputationBounds[Boolean] ; 

  /**
   * scala version of getBoolean
   **/
  @inline
  def boolean_! : ComputationBounds[Boolean] = getBoolean;

  /**
   * is this is byte ?
   **/
  def isByte:  ComputationBounds[Boolean];

  /**
   * get byte value if one is byte.
   */
  def getByte: ComputationBounds[Byte];

  /**
   * get byte value if one is byte.
   */
  @inline
  def byte_! : ComputationBounds[Byte] = getByte;

  /**
   *  is this is short ?
   **/
  def isShort:  ComputationBounds[Boolean];

  /**
   *  get short value.
   **/
  def getShort: ComputationBounds[Short];

  /**
   * scala version of getShort.
   **/
  @inline
  def short_! : ComputationBounds[Short] = getShort;
  
  /**
   *  is this is integer ?
   **/
  def isInt: ComputationBounds[Boolean];

  /**
   * get int value
   **/
  def getInt: ComputationBounds[Int];

  /**
   * scala version of getInt
   **/
  @inline
  def int_! : ComputationBounds[Int] = getInt;

  /**
   * is this is long ?
   **/
  def isLong:  ComputationBounds[Boolean];

  /**
   * get long is one is long.
   **/
  def getLong: ComputationBounds[Long];

  /**
   * if this is big integer
   **/
  def isBigInt: ComputationBounds[Boolean];

  /**
   * get BigInteger is one is big integer
   **/
  def getBigInt: ComputationBounds[BigInt];

  /**
   * get big intt is term have appropriate type.
   **/
  @inline
  def bigInt_! : ComputationBounds[BigInt] = getBigInt;

  /**
   * it this term can be represented as big decimal ?
   **/
  def isBigDecimal: ComputationBounds[Boolean];

  /**
   * get BigDecmal is term have approprate type.
   **/
  def getBigDecimal: ComputationBounds[BigDecimal];

  /**
   * get bigDecimal is term have appropriate type.
   **/
  @inline
  def bigDecimal_! : ComputationBounds[BigDecimal] = getBigDecimal;

  /**
   * if this is float.
   **/
  def isFloat: ComputationBounds[Boolean];

  def getFloat: ComputationBounds[Float];

  def isDouble: ComputationBounds[Boolean];

  def getDouble: ComputationBounds[Double];

  /**
   * if this is number type ?
   **/
  def isNumber: ComputationBounds[Boolean] ;

  def getNumber: ComputationBounds[Number];

  def getNumberKind: ComputationBounds[Int];

  def isChar:  ComputationBounds[Boolean] ;

  def getChar: ComputationBounds[Char];

  def isString:  ComputationBounds[Boolean] ;

  def getString: ComputationBounds[String] ;

  def string_! : ComputationBounds[String] = getString;

  /**
   * is this a special exception object ?
   **/
  def isException: ComputationBounds[Boolean] ;

  def getException: ComputationBounds[Exception] ;

  def getMessage: ComputationBounds[String];

  /**
   * is this is reference object ?
   * (i. e. derived from scala AnyRef or Java Object)
   **/
  def isRef: ComputationBounds[Boolean];

  def getRef: ComputationBounds[AnyRef];

  def ref_! : ComputationBounds[AnyRef] = getRef;

}
