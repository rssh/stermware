package ua.gradsoft.termware;

import java.lang.Number;

/**
 * API for access values, wrapped in terms.
 */
trait TermValue extends GeneralUtil
{

  /**
   * is this is a value term of type t ?	
   */
  @inline	
  def isValue[T](implicit mt:Manifest[T]) : Boolean = optValue[T].isDefined
  
  /**
   * get the value of type T, if term holds one
   * otherwise throws UnsupportedOperationException
   */
  def value[T](implicit mt:Manifest[T]): T =
  {
    optValue[T] match {
  	  case Some(x) => x
  	  case None => throwUOE
    }
  }
  
  /**
   * get the value of type t if term holds one
   * otherwise - nothing.
   */
  def optValue[T](implicit mt:Manifest[T]): Option[T]
	
  /**
   * is this is boolean ?
   */
  def isBoolean: Boolean;

  /**
   * get boolean value if one is boolean.
   */
  def getBoolean: Boolean;

  /**
   * scala version of getBoolean
   **/
  @inline
  final def boolean_! : Boolean = getBoolean;

  /**
   * is this is byte ?
   **/
  def isByte:  Boolean;

  /**
   * get byte value if one is byte.
   */
  def getByte: Byte;

  /**
   * get byte value if one is byte.
   */
  @inline
  final def byte_! : Byte = getByte;

  /**
   *  is this is short ?
   **/
  def isShort:  Boolean;

  /**
   *  get short value.
   **/
  def getShort: Short;

  /**
   * scala-like getShort
   **/
  @inline
  final def short_! : Short = getShort;


  /**
   *  is this is integer ?
   **/
  def isInt:  Boolean;

  /**
   * get int value
   **/
  def getInt: Int;

  /**
   * get int value
   **/
  @inline
  final def int_! : Int = getInt;

  /**
   * is this is long ?
   **/
  def isLong:  Boolean;

  /**
   * get long is one is long.
   **/
  def getLong: Long;

  /**
   * get long is one is long.
   **/
  @inline
  def long_! : Long = getLong;

  /**
   * if this is big integer
   **/
  def isBigInt: Boolean;

  /**
   * get BigInteger is one is big integer
   **/
  def getBigInt: BigInt;

  /**
   * get BigInteger if one is big integer.
   **/
  @inline
  def bigInt_! : BigInt = getBigInt;

  /**
   * it this term can be represented as big decimal ?
   **/
  def isBigDecimal: Boolean;

  /**
   * return bigDecimal value if hold one.
   **/
  def getBigDecimal: BigDecimal;

  @inline
  def bigDecimal_! : BigDecimal = getBigDecimal;

  def isFloat: Boolean;

  def getFloat: Float;

  @inline
  def float_! : Float = getFloat;

  def isDouble: Boolean;

  def getDouble: Double;

  @inline
  final def double_! : Double = getDouble;

  /**
   * if this is number type ?
   **/
  def isNumber: Boolean ;

  def getNumber: Number;

  @inline
  final def number_! : Number = getNumber;

  def getNumberKind: Int;

  def isChar:  Boolean = false;

  def getChar: Char = throwUOE;

  @inline
  final def char_! : Char = getChar;

  /**
   * is this is string ?
   **/
  def isString:  Boolean = false;

  def getString: String = throwUOE;

  @inline
  final def string_! : String = getString;

  /**
   * is this a special exception object ?
   **/
  def isException: Boolean = false;

  def getException: Exception = throwUOE;

  def getMessage: String = throwUOE;

  /**
   * is this is reference object ?
   * (i. e. derived from scala AnyRef or Java Object)
   **/
  def isRef: Boolean = false;

  def getRef: AnyRef = None;

}
