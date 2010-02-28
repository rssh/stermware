package ua.gradsoft.termware;


final case class BigIntName(v: BigInt) extends Name
{
 def getKindIndex: Int = NameKindIndex.forBigInt;
 def getIndex: Int = 0;
 def getString: String = value.toString;

 override def compare(that: Name):Int =
   if (getKindIndex == that.getKindIndex)
        getString.compare(that.getString);
   else
        getKindIndex - that.getKindIndex
   ;

 val value=v;
}


case class BigIntTerm(v:BigInt, s:BigIntTermSignature) 
                                                   extends PrimitiveTerm
{

  override def isBigInt: Boolean = true;

  override def getBigInt: Option[BigInt] = Some(value);

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return value.compare(t.getBigInt.get);
  }

  def termClassIndex: Int = TermClassIndex.NUMBER;

  lazy val name = new BigIntName(value);
  lazy val termHashCode = value.hashCode;
  val signature = s;
  val value = v;
}

