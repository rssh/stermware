package ua.gradsoft.termware;

import java.lang.Number;

final case class IntName(v: Int) extends Name
{
 def getKindIndex: Int = NameKindIndex.INT.id;
 def getIndex: Int = value;
 def getString: String = value.toString;

 override def compare(that: Name):Int =
   if (getKindIndex == that.getKindIndex)
        value - that.getIndex;
   else
        getKindIndex - that.getKindIndex
   ;

 val value=v;
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

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return (value - t.getInt);
  }


  lazy val name = new IntName(value);
  lazy val termHashCode = value.toInt;
}

