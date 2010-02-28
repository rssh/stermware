package ua.gradsoft.termware;

final case class BigDecimalName(v: BigDecimal) extends Name
{
 def getKindIndex: Int = NameKindIndex.forBigDecimal;
 def getIndex: Int = 0;
 def getString: String = value.toString;

 override def compare(that: Name):Int =
   if (getKindIndex == that.getKindIndex)
        getString.compareTo(that.getString);
   else
        getKindIndex - that.getKindIndex
   ;

 val value=v;
}


case class BigDecimalTerm(v:BigDecimal, s:BigDecimalTermSignature) 
                                                   extends PrimitiveTerm
{

  override def isBigDecimal: Boolean = true;

  override def getBigDecimal: Option[BigDecimal] = Some(value);

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return value.compare(t.getBigDecimal.get);
  }

  def termClassIndex: Int = TermClassIndex.NUMBER;

  lazy val name = new BigDecimalName(value);
  lazy val termHashCode = value.hashCode;
  val signature = s;
  val value = v;
}

