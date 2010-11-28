package ua.gradsoft.termware;

import java.lang.Number;

/**
 * Term which holds long value
 **/
case class FloatTerm(v:Float, s:FloatTermSignature) 
                               extends NumberPrimitiveTerm[Float](v,s)
{

  override def isByte: Boolean = 
         (value.toByte.toFloat==value);
  override def getByte: Byte =
          if (isByte) value.toByte else throwUOE;

  override def isShort: Boolean = 
         (value.toShort.toFloat==value);
  override def getShort: Short =
          if (isShort) value.toShort else throwUOE;

  override def isInt: Boolean = 
         (value.toInt.toFloat==value);
  override def getInt: Int =
          if (isInt) value.toInt else throwUOE;

  override def isLong: Boolean = 
         (value.toLong.toFloat==value);
  override def getLong: Long = 
          if (isLong) value.toLong else throwUOE;

  override def isFloat:  Boolean = true;
  override def getFloat: Float = value;

  override def isDouble:  Boolean = true;
  override def getDouble: Double = value.toDouble;

  override def isBigInt:  Boolean = 
         (value.toLong.toFloat==value);
  override def getBigInt: BigInt = 
     new BigInt(java.math.BigInteger.valueOf(value.toLong));

  override def isBigDecimal:  Boolean = true;
  override def getBigDecimal: BigDecimal = 
     new BigDecimal(new java.math.BigDecimal(value.toDouble));

  override def getNumber: Number = new java.lang.Float(value);
  override def getNumberKind: Int = NumberKind.FLOAT.id;

  def fixTermEq(t:Term):Boolean = t.isFloat && t.getFloat == value ;

  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    return (value - t.getFloat).toInt;
  }


  lazy val name = new StringName[java.lang.Float](new java.lang.Float(value),
                                         NameKindIndex.FLOAT.id);
  lazy val termHashCode = value.toInt;
}

