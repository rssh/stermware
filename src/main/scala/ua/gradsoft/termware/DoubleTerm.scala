package ua.gradsoft.termware;

import java.lang.Number;

/**
 * Term which holds long value
 **/
case class DoubleTerm(v:Double, s:DoubleTermSignature) 
                               extends NumberPrimitiveTerm[Double](v,s)
{

  override def isByte: Boolean = 
         (value.toByte.toDouble==value);
  override def getByte: Option[Byte] =
          if (isByte) Some(value.toByte) else None;

  override def isShort: Boolean = 
         (value.toShort.toDouble==value);
  override def getShort: Option[Short] =
          if (isShort) Some(value.toShort) else None;

  override def isInt: Boolean = 
         (value.toInt.toDouble==value);
  override def getInt: Option[Int] =
          if (isInt) Some(value.toInt) else None;

  override def isLong: Boolean = 
         (value.toLong.toDouble==value);
  override def getLong: Option[Long] = 
          if (isLong) Some(value.toLong) else None;

  override def isFloat:  Boolean = true;
  override def getFloat: Option[Float] = Some(value.toFloat);

  override def isDouble:  Boolean = true;
  override def getDouble: Option[Double] = Some(value);

  override def isBigInt:  Boolean = 
         (value.toLong.toDouble==value);
  override def getBigInt: Option[BigInt] = 
     Some(new BigInt(java.math.BigInteger.valueOf(value.toLong)));

  override def isBigDecimal:  Boolean = true;
  override def getBigDecimal: Option[BigDecimal] = 
     Some(new BigDecimal(new java.math.BigDecimal(value)));

  override def getNumber: Some[Number] = Some(new java.lang.Double(value));
  override def getNumberKind: Some[Int] = Some(NumberKind.DOUBLE.id);

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return (value - t.getDouble.get).toInt;
  }


  lazy val name = new StringName[java.lang.Double](new java.lang.Double(value),
                                         NameKindIndex.FLOAT.id);
  lazy val termHashCode = value.toInt;
}

