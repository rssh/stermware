package ua.gradsoft.termware;




case class StringTerm(v:String, s: StringTermSignature) 
                                            extends PrimitiveTerm(s)
                                               with NonNumberTerm
{

  override def isString: Boolean = true;
  override def getString: Option[String] = Some(value);

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return v.compare(t.getString.get);
  }

  def termClassIndex: Int = TermClassIndex.STRING;

  lazy val name = signature.theory.symbolTable.getOrCreate(
                                                    "\""+value+"\""
                                                          );

  lazy val termHashCode = value.hashCode;

  val value = v;
}

