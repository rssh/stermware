package ua.gradsoft.termware;

import java.io.PrintWriter;

class BigDecimalTerm(v:BigDecimal, s:BigDecimalTermSignature) 
                             extends NumberPrimitiveTerm[BigDecimal](v,s)
{
	
	
  override def isByte: Boolean =
         ( v == v.byteValue );
  override def getByte: Byte =
          if (isByte) v.byteValue else throwUOE;

  override def isShort: Boolean =
         (v == v.shortValue);
  override def getShort: Short =
          if (isShort) v.shortValue else throwUOE;

  override def isInt: Boolean =
         (v==v.intValue);
  override def getInt: Int =
          if (isInt) v.intValue else throwUOE;

  override def isLong: Boolean =
             (v==v.longValue);
  override def getLong: Long =
             if (isLong) v.longValue else throwUOE;

  override def isFloat: Boolean =
             (v==v.floatValue);
  override def getFloat: Float =
             if (isFloat) v.floatValue else throwUOE;

  override def isDouble: Boolean =
             (v==v.doubleValue);
  override def getDouble: Double =
             if (isDouble) v.doubleValue else throwUOE;

  override def isBigInt: Boolean = 
             (v==v.toBigInt);
  override def getBigInt: BigInt = 
             if (isBigInt) v.toBigInt else throwUOE;

  override def isBigDecimal: Boolean = true;
  override def getBigDecimal: BigDecimal = v;

  override def getNumber: Number = v.bigDecimal;
  override def getNumberKind: Int = NumberKind.BIG_DECIMAL.id;

  def fixTermEq(t:Term):Boolean = t.isBigDecimal && t.getBigDecimal == v ;

  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    return v.compare(t.getBigDecimal);
  }


  lazy val name = 
            new StringName[BigDecimal](v,NameKindIndex.BIG_DECIMAL.id);

  lazy val termHashCode = v.hashCode;

  override def print(out:PrintWriter):Unit = { out.print(v); }


}

object BigDecimalTerm
{
  def apply(v:BigDecimal, s:BigDecimalTermSignature):BigDecimalTerm
                                                 = new BigDecimalTerm(v,s);
  
  def unapply(x:Term):Option[Pair[BigDecimal,TermSignature]] = 
  	  if (x.isBigDecimal) Some((x.getBigDecimal,x.signature)) else None
}

