package ua.gradsoft.termware;

import java.lang.Number;

/**
 * Term which holds long value
 **/
case class FloatTerm(override val v: Float, s:FloatTermSignature) 
                               extends NumberPrimitiveTerm[Float](v,s)
{

  override def isByte: Boolean = 
         (v.toByte.toFloat==v);
  override def getByte: Byte =
          if (isByte) v.toByte else throwUOE;

  override def isShort: Boolean = 
         (v.toShort.toFloat==v);
  override def getShort: Short =
          if (isShort) v.toShort else throwUOE;

  override def isInt: Boolean = 
         (v.toInt.toFloat==v);
  override def getInt: Int =
          if (isInt) v.toInt else throwUOE;

  override def isLong: Boolean = 
         (v.toLong.toFloat==v);
  override def getLong: Long = 
          if (isLong) v.toLong else throwUOE;

  override def isFloat:  Boolean = true;
  override def getFloat: Float = v;

  override def isDouble:  Boolean = true;
  override def getDouble: Double = v.toDouble;

  override def isBigInt:  Boolean = 
         (v.toLong.toFloat==v);
  override def getBigInt: BigInt = 
     new BigInt(java.math.BigInteger.valueOf(v.toLong));

  override def isBigDecimal:  Boolean = true;
  override def getBigDecimal: BigDecimal = 
     new BigDecimal(new java.math.BigDecimal(v.toDouble));

  override def getNumber: Number = new java.lang.Float(v);
  override def getNumberKind: Int = NumberKind.FLOAT.id;

  def fixTermEq(t:Term):Boolean = t.isFloat && t.getFloat == v ;

  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    return (v - t.getFloat).toInt;
  }


  lazy val name = new StringName[java.lang.Float](new java.lang.Float(v),
                                         NameKindIndex.FLOAT.id);
  lazy val termHashCode = v.toInt;
}

