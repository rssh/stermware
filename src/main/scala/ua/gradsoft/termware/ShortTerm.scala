package ua.gradsoft.termware;

import java.lang.Number;

/**
 * short constant as term.
 **/
case class ShortTerm(v:Short, s:ShortTermSignature) 
                               extends NumberPrimitiveTerm[Short](v,s)
{

  override def isByte: Boolean = (value.toByte.toShort==value);
  override def getByte: Option[Byte] =
    if (isByte) Some(value.toByte)  else None;

  override def isShort: Boolean = true;
  override def getShort: Option[Short] = Some(value);

  override def isInt: Boolean = true;
  override def getInt: Option[Int] = Some(value.toInt);

  override def isLong: Boolean = true;
  override def getLong: Option[Long] = Some(value.toLong);

  override def isFloat: Boolean = true;
  override def getFloat: Option[Float] = Some(value.toFloat);

  override def isDouble: Boolean = true;
  override def getDouble: Option[Double] = Some(value.toDouble);

  override def isBigInt: Boolean = true;
  override def getBigInt: Option[BigInt] = 
    Some(new BigInt(java.math.BigInteger.valueOf(value.toLong)));

  override def isBigDecimal: Boolean = true;
  override def getBigDecimal: Option[BigDecimal] = 
     Some(new BigDecimal(new java.math.BigDecimal(value.toInt)));

  override def getNumber: Option[Number] = Some(new java.lang.Short(value));
  override def getNumberKind: Option[Int] = Some(NumberKind.SHORT.id);

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return (value - t.getShort.get);
  }

  lazy val name = new IntName(value.toInt);
  lazy val termHashCode = value.toInt;

}

