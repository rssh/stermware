package ua.gradsoft.termware;

import java.lang.Number;

/**
 * Term which holds long value
 **/
case class LongTerm(v:Long, s:LongTermSignature) 
                                  extends NumberPrimitiveTerm[Long](v,s)
{

  override def isByte: Boolean = 
         (value.toByte.toLong==value);
  override def getByte: Byte =
          if (isByte) value.toByte else throwUOE;

  override def isShort: Boolean = 
         (value.toShort.toLong==value);
  override def getShort: Short =
          if (isShort) value.toShort else throwUOE;

  override def isInt: Boolean = 
         (value.toInt.toLong==value);
  override def getInt: Int =
          if (isInt) value.toInt else throwUOE;

  override def isLong: Boolean = true;
  override def getLong: Long = value;

  override def isFloat:  Boolean = true;
  override def getFloat: Float = value.toFloat;

  override def isDouble:  Boolean = true;
  override def getDouble: Double = value.toDouble;

  override def isBigInt:  Boolean = true;
  override def getBigInt: BigInt = 
     new BigInt(java.math.BigInteger.valueOf(value));

  override def isBigDecimal:  Boolean = true;
  override def getBigDecimal: BigDecimal = 
     new BigDecimal(new java.math.BigDecimal(value));

  override def getNumber: Number = new java.lang.Long(value);
  override def getNumberKind: Int = NumberKind.LONG.id;


  def fixTermEq(t:Term):Boolean = t.isLong && t.getLong == value ;

  def fixTermCompare(t: Term):Int = {
    val c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    val cl = value - t.getLong;
    return if (value < 0)  -1 
           else if (value > 0) 1
           else 0;
  }


  lazy val name = new StringName[java.lang.Long](new java.lang.Long(value),
                                                 NameKindIndex.LONG.id
                                                );
  lazy val termHashCode = value.toInt;
}

