package ua.gradsoft.termware;


case class RefTerm(v:AnyRef, s: RefTermSignature) extends PrimitiveTerm(s)
                                           with NonNumberTerm
{

  override def isRef: Boolean = true;

  override def getRef: AnyRef = value;

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    if (value!=null) {
      if (value eq t.getRef) {
         return 0;
      }
      c = value.hashCode - t.getRef.hashCode;
      if (c!=0) return c;
      // equal hash codes with special names: near impossible, but can be specially constructed.
      // in such (very rare) case compare string representations.
      return value.toString.compare(t.getRef.toString);
    } else {
      return if (t.getRef==null)  0 else -1 ;
    }
  }

  def termClassIndex: Int = TermClassIndex.BOOLEAN;

  lazy val name = signature.theory.symbolTable.getOrCreate(
                                                      "@"+value.hashCode
                                                          );

  lazy val termHashCode = v.hashCode;

  val value = v;
}

