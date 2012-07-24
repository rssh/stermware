package ua.gradsoft.termware;

import java.lang.Number;

/**
 * Term which holds long value
 **/
case class LongTerm(override val v:Long, s:LongTermSignature) 
                                  extends NumberPrimitiveTerm[Long](v,s)
{

	
  override def isByte: Boolean = 
         (v.toByte.toLong==v);
  override def getByte: Byte =
          if (isByte) v.toByte else throwUOE;

  override def isShort: Boolean = 
         (v.toShort.toLong==v);
  override def getShort: Short =
          if (isShort) v.toShort else throwUOE;

  override def isInt: Boolean = 
         (v.toInt.toLong==v);
  override def getInt: Int =
          if (isInt) v.toInt else throwUOE;

  override def isLong: Boolean = true;
  override def getLong: Long = v;

  override def isFloat:  Boolean = true;
  override def getFloat: Float = v.toFloat;

  override def isDouble:  Boolean = true;
  override def getDouble: Double = v.toDouble;

  override def isBigInt:  Boolean = true;
  override def getBigInt: BigInt = 
     new BigInt(java.math.BigInteger.valueOf(v));

  override def isBigDecimal:  Boolean = true;
  override def getBigDecimal: BigDecimal = 
     new BigDecimal(new java.math.BigDecimal(v));

  override def getNumber: Number = new java.lang.Long(v);
  override def getNumberKind: Int = NumberKind.LONG.id;


  def fixTermEq(t:Term):Boolean = t.isLong && t.getLong == v ;

  def fixTermCompare(t: Term):Int = {
    val c = termClassIndex - t.termClassIndex;
    if (c!=0) {
      c	
    } else {
      (v - t.getLong).signum;
    }; 
  }


  lazy val name = new StringName[java.lang.Long](new java.lang.Long(v),
                                                 NameKindIndex.LONG.id
                                                );
  lazy val termHashCode = v.toInt;
}

