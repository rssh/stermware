package ua.gradsoft.termware;


/**
 * API for access values, wrapped in terms.
 */
trait NonNumberTerm
{
  this: Term =>

  def isByte:  Boolean = false;

  def getByte: Byte = throwUOE;

  def isShort:  Boolean = false;

  def getShort:  Short = throwUOE;

  def isInt:  Boolean = false;

  def getInt: Int = throwUOE;

  def isLong:  Boolean = false;

  def getLong: Long = throwUOE;

  def isBigInt: Boolean = false;

  def getBigInt: BigInt = throwUOE;

  def isBigDecimal: Boolean = false;

  def getBigDecimal: BigDecimal = throwUOE;

  def isFloat: Boolean = false;

  def getFloat: Float = throwUOE;

  def isDouble: Boolean = false;

  def getDouble: Double = throwUOE;

  def isNumber: Boolean = false ;

  def getNumber: Number = throwUOE;

  def getNumberKind: Int =  throwUOE;

}
