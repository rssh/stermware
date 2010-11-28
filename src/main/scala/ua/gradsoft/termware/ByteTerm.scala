package ua.gradsoft.termware;


/**
 * short constant as byte
 **/
case class ByteTerm(v:Byte, s:ByteTermSignature) 
                               extends NumberPrimitiveTerm[Byte](v,s)
{

  override def isByte: Boolean = true;
  override def getByte: Byte = value;

  override def isShort: Boolean = true;
  override def getShort: Short = value.toShort;

  override def isInt: Boolean = true;
  override def getInt: Int = value.toInt;

  override def isLong: Boolean = true;
  override def getLong: Long = value.toLong;

  override def isFloat: Boolean = true;
  override def getFloat: Float = value.toFloat;

  override def isDouble: Boolean = true;
  override def getDouble: Double = value.toDouble;

  override def isBigInt: Boolean = true;
  override def getBigInt: BigInt =
    new BigInt(java.math.BigInteger.valueOf(value.toLong));

  override def isBigDecimal: Boolean = true;
  override def getBigDecimal: BigDecimal =
     new BigDecimal(new java.math.BigDecimal(value.toInt));

  override def getNumber: Number = new java.lang.Byte(value);
  override def getNumberKind: Int = NumberKind.BYTE.id;

  def fixTermEq(t: Term):Boolean = t.isByte && t.getByte == value;

  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    return (value - t.getByte).toInt;
  }


  lazy val name = new IntName(value.toInt);
  lazy val termHashCode = value.toInt;
}

