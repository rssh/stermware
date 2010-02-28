package ua.gradsoft.termware;


/**
 * API for access values, wrapped in terms.
 */
trait NonNumberTerm
{
  this: Term =>

  def isByte:  Boolean = false;

  def getByte: Option[Byte] = None;

  def isShort:  Boolean = false;

  def getShort:  Option[Short] = None;

  def isInt:  Boolean = false;

  def getInt: Option[Int] = None;

  def isLong:  Boolean = false;

  def getLong: Option[Long] = None;

  def isBigInt: Boolean = false;

  def getBigInt: Option[BigInt] = None;

  def isBigDecimal: Boolean = false;

  def getBigDecimal: Option[BigDecimal] = None;

  def isFloat: Boolean = false;

  def getFloat: Option[Float] = None;

  def isDouble: Boolean = false;

  def getDouble: Option[Double] = None;

  def isNumber: Boolean = false ;

  def getNumber: Option[Number] = None;

  def getNumberKind: Option[Int] = None;

}
