package ua.gradsoft.termware;

import java.lang.Number;

case class BigIntTerm(v:BigInt, s:BigIntTermSignature) 
                               extends NumberPrimitiveTerm[BigInt](v,s)
{

  override def isByte: Boolean =
         ( value == value.byteValue );
  override def getByte: Byte =
          if (isByte) value.byteValue else throwUOE;

  override def isShort: Boolean =
         (value == value.shortValue);
  override def getShort: Short =
          if (isShort) value.shortValue else throwUOE;

  override def isInt: Boolean =
         (value==value.intValue);
  override def getInt: Int =
          if (isInt) value.intValue else throwUOE;

  override def isLong: Boolean = 
             (value==value.longValue);
  override def getLong: Long = 
             if (isLong) value.longValue else throwUOE;

  override def isFloat: Boolean = 
             (value==value.floatValue);
  override def getFloat: Float = 
             if (isFloat) value.floatValue else throwUOE;

  override def isDouble: Boolean = 
             (value==value.doubleValue);
  override def getDouble: Double = 
             if (isDouble) value.doubleValue else throwUOE;

  override def isBigInt: Boolean = true;
  override def getBigInt: BigInt = value;

  override def isBigDecimal: Boolean = true;
  override def getBigDecimal: BigDecimal = 
    new BigDecimal(new java.math.BigDecimal(value.bigInteger));

  override def getNumber: Number = value.bigInteger;
  override def getNumberKind: Int = NumberKind.BIG_INTEGER.id;

  

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return value.compare(t.getBigInt);
  }

  lazy val name = new StringName[BigInt](value,NameKindIndex.BIG_INT.id);
  lazy val termHashCode = value.hashCode;
}

