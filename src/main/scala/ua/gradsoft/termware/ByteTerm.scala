package ua.gradsoft.termware;


/**
 * short constant as byte
 **/
case class ByteTerm(v:Byte, s:ByteTermSignature) 
                                               extends PrimitiveTerm
{

  override def isByte: Boolean = true;

  override def getByte: Option[Byte] = Some(value);

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return (value - t.getInt.get).toInt;
  }

  def termClassIndex: Int = TermClassIndex.NUMBER;

  lazy val name = new IntName(value.toInt);
  lazy val termHashCode = value.toInt;
  val signature = s;
  val value = v;
}

