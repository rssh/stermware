package ua.gradsoft.termware;

import java.lang.Number;

/**
 * Term which holds long value
 **/
case class DoubleTerm(override val v:Double, s:DoubleTermSignature) 
                               extends NumberPrimitiveTerm[Double](v,s)
{

  override def isByte: Boolean = 
         (v.toByte.toDouble==v);
  override def getByte: Byte =
          if (isByte) v.toByte else throwUOE;

  override def isShort: Boolean = 
         (v.toShort.toDouble==v);
  override def getShort: Short =
          if (isShort) v.toShort else throwUOE;

  override def isInt: Boolean = 
         (v.toInt.toDouble==v);
  override def getInt: Int =
          if (isInt) v.toInt else throwUOE;

  override def isLong: Boolean = 
         (v.toLong.toDouble==v);
  override def getLong: Long = 
          if (isLong) v.toLong else throwUOE;

  override def isFloat:  Boolean = true;
  override def getFloat: Float = v.toFloat;

  override def isDouble:  Boolean = true;
  override def getDouble: Double = v;

  override def isBigInt:  Boolean = 
         (v.toLong.toDouble==v);
  override def getBigInt: BigInt = 
     new BigInt(java.math.BigInteger.valueOf(v.toLong));

  override def isBigDecimal:  Boolean = true;
  override def getBigDecimal: BigDecimal = 
     new BigDecimal(new java.math.BigDecimal(v));

  override def getNumber: Number = new java.lang.Double(v);
  override def getNumberKind: Int = NumberKind.DOUBLE.id;

  def fixTermEq(t:Term):Boolean = t.isDouble && t.getDouble == v;

  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    return (v - t.getDouble).signum
  }


  lazy val name = new StringName[java.lang.Double](new java.lang.Double(v),
                                         NameKindIndex.FLOAT.id);
  lazy val termHashCode = v.toInt;
}

