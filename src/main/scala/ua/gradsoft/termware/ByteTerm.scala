package ua.gradsoft.termware;


/**
 * short constant as byte
 **/
case class ByteTerm(override val v:Byte, s:ByteTermSignature) 
                               extends NumberPrimitiveTerm[Byte](v,s)
{
		

  override def isByte: Boolean = true;
  override def getByte: Byte = v;

  override def isShort: Boolean = true;
  override def getShort: Short = v.toShort;

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

  override def getNumber: Number = new java.lang.Byte(v);
  override def getNumberKind: Int = NumberKind.BYTE.id;

  def fixTermEq(t: Term):Boolean = t.isByte && t.getByte == v;

  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    return (v - t.getByte).signum;
  }


  lazy val name = new IntName(v.toInt);
  lazy val termHashCode = v.toInt;
}


