package ua.gradsoft.termware;


case class BigDecimalTerm(v:BigDecimal, s:BigDecimalTermSignature) 
                             extends NumberPrimitiveTerm[BigDecimal](v,s)
{

  override def isByte: Boolean =
         ( value == value.byteValue );
  override def getByte: Option[Byte] =
          if (isByte) Some(value.byteValue) else None;

  override def isShort: Boolean =
         (value == value.shortValue);
  override def getShort: Option[Short] =
          if (isShort) Some(value.shortValue) else None;

  override def isInt: Boolean =
         (value==value.intValue);
  override def getInt: Option[Int] =
          if (isInt) Some(value.intValue) else None;

  override def isLong: Boolean =
             (value==value.longValue);
  override def getLong: Option[Long] =
             if (isLong) Some(value.longValue) else None;

  override def isFloat: Boolean =
             (value==value.floatValue);
  override def getFloat: Option[Float] =
             if (isFloat) Some(value.floatValue) else None;

  override def isDouble: Boolean =
             (value==value.doubleValue);
  override def getDouble: Option[Double] =
             if (isDouble) Some(value.doubleValue) else None;

  override def isBigInt: Boolean = 
             (value==value.toBigInt);
  override def getBigInt: Option[BigInt] = 
             if (isBigInt) Some(value.toBigInt) else None;

  override def isBigDecimal: Boolean = true;
  override def getBigDecimal: Option[BigDecimal] = Some(value);

  override def getNumber: Some[Number] = Some(value.bigDecimal);
  override def getNumberKind: Some[Int] = Some(NumberKind.BIG_DECIMAL.id);


  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return value.compare(t.getBigDecimal.get);
  }


  lazy val name = 
            new StringName[BigDecimal](value,NameKindIndex.BIG_DECIMAL.id);

  lazy val termHashCode = value.hashCode;
}

