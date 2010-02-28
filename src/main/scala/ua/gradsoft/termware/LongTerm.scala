package ua.gradsoft.termware;


final case class LongName(v: Long) extends Name
{
 def getKindIndex: Int = NameKindIndex.forLong;
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


case class LongTerm(v:Long, s:LongTermSignature) 
                                                   extends PrimitiveTerm
{

  override def isLong: Boolean = true;

  override def getLong: Option[Long] = Some(value);

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return (value - t.getLong.get).toInt;
  }

  def termClassIndex: Int = TermClassIndex.NUMBER;

  lazy val name = new LongName(value);
  lazy val termHashCode = value.toInt;
  val signature = s;
  val value = v;
}

