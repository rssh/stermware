package ua.gradsoft.termware;


case class RefTerm(v:AnyRef, s: RefTermSignature) extends PrimitiveTerm
{

  override def isRef: Boolean = true;

  override def getRef: Option[AnyRef] = Some(value);

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    if (value!=null) {
      if (value eq t.getRef.get) {
         return 0;
      }
      c = value.hashCode - t.getRef.get.hashCode;
      if (c!=0) return c;
      // equal hash codes with special names: near impossible, but can be specially constructed.
      // in such (very rare) case compare string representations.
      return value.toString.compare(t.getRef.toString);
    } else {
      return if (t.getRef.get==null)  0 else -1 ;
    }
  }

  def termClassIndex: Int = TermClassIndex.BOOLEAN;

  lazy val name = signature.theory.symbolTable.getOrCreate(
                                                      "@"+value.hashCode
                                                          );

  lazy val termHashCode = name.hashCode;

  val signature = s;
  val value = v;
}

