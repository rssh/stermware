package ua.gradsoft.termware;

case class CharName(v:Char) extends Name
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


case class CharTerm(v:Char, s: CharTermSignature) extends PrimitiveTerm
{

  override def isChar: Boolean = true;

  override def getChar: Option[Char] = Some(value);

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return v.compare(t.getChar.get);
  }

  def termClassIndex: Int = TermClassIndex.CHAR;

  lazy val name = new CharName(value);

  lazy val termHashCode = value.hashCode;

  val signature = s;
  val value = v;
}

