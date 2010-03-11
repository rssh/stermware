package ua.gradsoft.termware;

import java.lang.Number;

/**
 * short constant as term.
 **/
case class ShortTerm(v:Short, s:ShortTermSignature) 
                               extends NumberPrimitiveTerm[Short](v,s)
{

  override def isByte: Boolean = (value.toByte.toShort==value);
  override def getByte: Byte =
    if (isByte) value.toByte  else throwUOE;

  override def isShort: Boolean = true;
  override def getShort: Short = value;

  override def isInt: Boolean = true;
  override def getInt: Int = value.toInt;

  override def isLong: Boolean = true;
  override def getLong: Long = value.toLong;

  override def isFloat: Boolean = true;
  override def getFloat: Float = value.toFloat;

  override def isDouble: Boolean = true;
  override def getDouble: Double = value.toDouble;

  override def isBigInt: Boolean = true;
  override def getBigInt: BigInt = 
    new BigInt(java.math.BigInteger.valueOf(value.toLong));

  override def isBigDecimal: Boolean = true;
  override def getBigDecimal: BigDecimal = 
     new BigDecimal(new java.math.BigDecimal(value.toInt));

  override def getNumber: Number = new java.lang.Short(value);
  override def getNumberKind: Int = NumberKind.SHORT.id;

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return (value - t.getShort);
  }

  lazy val name = new IntName(value.toInt);
  lazy val termHashCode = value.toInt;

}

