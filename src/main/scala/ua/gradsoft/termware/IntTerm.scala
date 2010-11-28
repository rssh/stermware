package ua.gradsoft.termware;

import java.lang.Number;

final case class IntName(v: Int) extends Name
{
 def kindIndex: Int = NameKindIndex.INT.id;
 def index: Int = v;
 def string: String = v.toString;

 override def compare(that: Name):Int =
   if (kindIndex == that.kindIndex)
        v - that.index;
   else
        kindIndex - that.kindIndex
   ;

}

case class IntTerm(v:Int, s:IntTermSignature) 
                               extends NumberPrimitiveTerm[Int](v,s)
{

  override def isByte: Boolean =
           (value.toByte.toInt == value);
  override def getByte: Byte =
      if (isByte) value.toByte else throwUOE;

  override def isShort: Boolean =
           (value.toShort.toInt == value);
  override def getShort: Short =
      if (isShort) value.toShort else throwUOE;

  override def isInt: Boolean = true;
  override def getInt: Int = value;

  override def isLong: Boolean = true;
  override def getLong: Long = value.toLong;

  override def isFloat: Boolean = true;
  override def getFloat: Float = value.toFloat;

  override def isDouble: Boolean = true;
  override def getDouble: Double = value.toDouble;

  override def isBigInt:  Boolean = true;
  override def getBigInt: BigInt =
     new BigInt(java.math.BigInteger.valueOf(value));

  override def isBigDecimal:  Boolean = true;
  override def getBigDecimal: BigDecimal =
     new BigDecimal(new java.math.BigDecimal(value));

  override def getNumber: Number = new java.lang.Integer(value);
  override def getNumberKind: Int = NumberKind.INT.id;

  def fixTermEq(t:Term):Boolean = t.isInt && t.getInt == value ;

  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    return (value - t.getInt);
  }

  override def toString = value.toString;

  lazy val name = new IntName(value);
  lazy val termHashCode = value.toInt;
}

