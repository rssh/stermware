package ua.gradsoft.termware;

import java.lang.Number;

/**
 * Class of names, which represents Int-s
 */
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

/**
 * Term, represented int values
 */
case class IntTerm(override val v:Int, s:IntTermSignature) 
                               extends NumberPrimitiveTerm[Int](v,s)
{

  override def isByte: Boolean =
           (v.toByte.toInt == v);
  override def getByte: Byte =
      if (isByte) v.toByte else throwUOE;

  override def isShort: Boolean =
           (v.toShort.toInt == v);
  override def getShort: Short =
      if (isShort) v.toShort else throwUOE;

  override def isInt: Boolean = true;
  override def getInt: Int = v;

  override def isLong: Boolean = true;
  override def getLong: Long = v.toLong;

  override def isFloat: Boolean = true;
  override def getFloat: Float = v.toFloat;

  override def isDouble: Boolean = true;
  override def getDouble: Double = v.toDouble;

  override def isBigInt:  Boolean = true;
  override def getBigInt: BigInt =
     new BigInt(java.math.BigInteger.valueOf(v));

  override def isBigDecimal:  Boolean = true;
  override def getBigDecimal: BigDecimal =
     new BigDecimal(new java.math.BigDecimal(v));

  override def getNumber: Number = new java.lang.Integer(v);
  override def getNumberKind: Int = NumberKind.INT.id;

  def fixTermEq(t:Term):Boolean = t.isInt && t.getInt == v ;

  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    return (v - t.getInt);
  }

  override def toString = value.toString;

  lazy val name = new IntName(v);
  lazy val termHashCode = v.toInt;
}

