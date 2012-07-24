package ua.gradsoft.termware;

import java.lang.Number;

/**
 * short constant as term.
 **/
case class ShortTerm(override val v:Short, s:ShortTermSignature) 
                               extends NumberPrimitiveTerm[Short](v,s)
{
	
  override def isByte: Boolean = (v.toByte.toShort==v);
  override def getByte: Byte =
    if (isByte) v.toByte  else throwUOE;

  override def isShort: Boolean = true;
  override def getShort: Short = v;

  override def isInt: Boolean = true;
  override def getInt: Int = v.toInt;

  override def isLong: Boolean = true;
  override def getLong: Long = v.toLong;

  override def isFloat: Boolean = true;
  override def getFloat: Float = v.toFloat;

  override def isDouble: Boolean = true;
  override def getDouble: Double = v.toDouble;

  override def isBigInt: Boolean = true;
  override def getBigInt: BigInt = 
    new BigInt(java.math.BigInteger.valueOf(v.toLong));

  override def isBigDecimal: Boolean = true;
  override def getBigDecimal: BigDecimal = 
     new BigDecimal(new java.math.BigDecimal(v.toInt));

  override def getNumber: Number = new java.lang.Short(v);
  override def getNumberKind: Int = NumberKind.SHORT.id;

  def fixTermCompare(t: Term):Int = {
    val c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    return (v - t.getShort);
  }

  def fixTermEq(t:Term): Boolean = t.isShort && t.getShort == v;

  lazy val name = new IntName(v.toInt);
  lazy val termHashCode = v.toInt;

}

