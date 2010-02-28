package ua.gradsoft.termware;


case class BooleanTerm(v:Boolean, s: BooleanTermSignature) 
                                          extends PrimitiveTerm(s)
                                            with NonNumberTerm
{

  override def isBoolean: Boolean = true;

  override def getBoolean: Option[Boolean] = Some(value);

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    if (value) {
      return (if (t.getBoolean.get) 0 else 1);
    } else {
      return (if (t.getBoolean.get) -1 else 0);
    }
  }

  def termClassIndex: Int = TermClassIndex.BOOLEAN;

  lazy val name = signature.theory.symbolTable.getOrCreate(
                                        if (value) "true" else "false" 
                                                          );

  lazy val termHashCode = name.hashCode;

  val value = v;
}

