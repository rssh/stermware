package ua.gradsoft.termware;


final case class IntName(v: Int) extends Name
{
 def getKindIndex: Int = NameKindIndex.forLong;
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
                                                   extends PrimitiveTerm
{

  override def isInt: Boolean = true;

  override def getInt: Option[Int] = Some(value);

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return (value - t.getInt.get);
  }

  def termClassIndex: Int = TermClassIndex.NUMBER;

  lazy val name = new LongName(value);
  lazy val termHashCode = value.toInt;
  val signature = s;
  val value = v;
}

