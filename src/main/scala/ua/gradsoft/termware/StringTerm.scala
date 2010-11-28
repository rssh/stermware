package ua.gradsoft.termware;




case class StringTerm(v:String, s: StringTermSignature) 
                                            extends PrimitiveTerm(s)
                                               with NonNumberTerm
{

  override def isString: Boolean = true;
  override def getString: String = value;

  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return v.compare(t.getString);
  }

  def fixTermEq(t: Term):Boolean = t.isString && t.getString == value ;

  def termClassIndex: Int = TermClassIndex.STRING;

  lazy val name = new StringName[String](value,NameKindIndex.STRING.id);

  lazy val termHashCode = value.hashCode;

  val value = v;
}

