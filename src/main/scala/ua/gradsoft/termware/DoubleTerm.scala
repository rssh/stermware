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
  override def getByte: Byte =
          if (isByte) value.toByte else throwUOE;

  override def isShort: Boolean = 
         (value.toShort.toDouble==value);
  override def getShort: Short =
          if (isShort) value.toShort else throwUOE;

  override def isInt: Boolean = 
         (value.toInt.toDouble==value);
  override def getInt: Int =
          if (isInt) value.toInt else throwUOE;

  override def isLong: Boolean = 
         (value.toLong.toDouble==value);
  override def getLong: Long = 
          if (isLong) value.toLong else throwUOE;

  override def isFloat:  Boolean = true;
  override def getFloat: Float = value.toFloat;

  override def isDouble:  Boolean = true;
  override def getDouble: Double = value;

  override def isBigInt:  Boolean = 
         (value.toLong.toDouble==value);
  override def getBigInt: BigInt = 
     new BigInt(java.math.BigInteger.valueOf(value.toLong));

  override def isBigDecimal:  Boolean = true;
  override def getBigDecimal: BigDecimal = 
     new BigDecimal(new java.math.BigDecimal(value));

  override def getNumber: Number = new java.lang.Double(value);
  override def getNumberKind: Int = NumberKind.DOUBLE.id;

  def fixTermEq(t:Term):Boolean = t.isDouble && t.getDouble == value;

  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    return (value - t.getDouble).toInt;
  }


  lazy val name = new StringName[java.lang.Double](new java.lang.Double(value),
                                         NameKindIndex.FLOAT.id);
  lazy val termHashCode = value.toInt;
}

