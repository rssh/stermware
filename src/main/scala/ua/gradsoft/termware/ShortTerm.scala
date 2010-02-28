package ua.gradsoft.termware;


/**
 * short constant as term.
 **/
case class ShortTerm(v:Short, s:ShortTermSignature) 
                                                   extends PrimitiveTerm
{

  override def isShort: Boolean = true;

  override def getShort: Option[Short] = Some(value);

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return (value - t.getInt.get);
  }

  def termClassIndex: Int = TermClassIndex.NUMBER;

  lazy val name = new IntName(value.toInt);
  lazy val termHashCode = value.toInt;
  val signature = s;
  val value = v;
}

