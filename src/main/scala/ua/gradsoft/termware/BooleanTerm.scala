package ua.gradsoft.termware;

object BooleanTerm
{
   val TERM_CLASS_INDEX: Int = 5;
}

class BooleanTerm(v:Boolean, s: BooleanTermSignature) extends PrimitiveTerm
{

  override def isBoolean: Boolean = true;

  override def getBoolean: Option[Boolean] = Some(value);

  def isAtom = false;

  def isNil = false;

  def termCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    if (value) {
      return (if (t.getBoolean.get) 0 else 1);
    } else {
      return (if (t.getBoolean.get) -1 else 0);
    }
  }

  def termUnify(t: Term, s: Substitution)
   = 
     if (t.isBoolean) 
       if (t.getBoolean.get==value) (true,s)
       else (false,s)
     else
       if (t.isX) {
         val r = s+(t->this);
         (r._1, if (r._1) r._2 else s);
       } else
         (false, s)
   ;

  def termClassIndex: Int = BooleanTerm.TERM_CLASS_INDEX;

  lazy val name = signature.theory.symbolTable.getOrCreateElement(
                                        if (value) "true" else "false" );

  lazy val termHashCode = name.hashCode;

  val signature = s;
  val value = v;
}

