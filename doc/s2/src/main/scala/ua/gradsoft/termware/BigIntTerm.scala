package ua.gradsoft.termware;

import java.lang.Number;

case class BigIntTerm(override val v:BigInt, s:BigIntTermSignature) 
                               extends NumberPrimitiveTerm[BigInt](v,s)
{
	
  override def isByte: Boolean =
         ( v == v.byteValue );
  override def getByte: Byte =
          if (isByte) v.byteValue else throwUOE;

  override def isShort: Boolean =
         (v == v.shortValue);
  override def getShort: Short =
          if (isShort) v.shortValue else throwUOE;

  override def isInt: Boolean =
         (v==v.intValue);
  override def getInt: Int =
          if (isInt) v.intValue else throwUOE;

  override def isLong: Boolean = 
             (v==v.longValue);
  override def getLong: Long = 
             if (isLong) v.longValue else throwUOE;

  override def isFloat: Boolean = 
             (v==v.floatValue);
  override def getFloat: Float = 
             if (isFloat) v.floatValue else throwUOE;

  override def isDouble: Boolean = 
             (v==v.doubleValue);
  override def getDouble: Double = 
             if (isDouble) v.doubleValue else throwUOE;

  override def isBigInt: Boolean = true;
  override def getBigInt: BigInt = v;

  override def isBigDecimal: Boolean = true;
  override def getBigDecimal: BigDecimal = 
    new BigDecimal(new java.math.BigDecimal(v.bigInteger));

  override def getNumber: Number = v.bigInteger;
  override def getNumberKind: Int = NumberKind.BIG_INTEGER.id;

  def fixTermEq(t:Term):Boolean = t.isBigDecimal && v==t.getBigDecimal;
  
  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    return v.compare(t.getBigInt);
  }

  lazy val name = new StringName[BigInt](v,NameKindIndex.BIG_INT.id);
  lazy val termHashCode = v.hashCode;
}

