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
  override def getByte: Option[Byte] =
      if (isByte) Some(value.toByte) else None;

  override def isShort: Boolean =
           (value.toShort.toInt == value);
  override def getShort: Option[Short] =
      if (isShort) Some(value.toShort) else None;

  override def isInt: Boolean = true;
  override def getInt: Option[Int] = Some(value);

  override def isLong: Boolean = true;
  override def getLong: Option[Long] = Some(value.toLong);

  override def isFloat: Boolean = true;
  override def getFloat: Option[Float] = Some(value.toFloat);

  override def isDouble: Boolean = true;
  override def getDouble: Option[Double] = Some(value.toDouble);

  override def isBigInt:  Boolean = true;
  override def getBigInt: Option[BigInt] =
     Some(new BigInt(java.math.BigInteger.valueOf(value)));

  override def isBigDecimal:  Boolean = true;
  override def getBigDecimal: Option[BigDecimal] =
     Some(new BigDecimal(new java.math.BigDecimal(value)));

  override def getNumber: Option[Number] = Some(new java.lang.Integer(value));
  override def getNumberKind: Option[Int] = Some(NumberKind.INT.id);

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return (value - t.getInt.get);
  }


  lazy val name = new LongName(value);
  lazy val termHashCode = value.toInt;
}

