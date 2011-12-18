package ua.gradsoft.termware;

import java.io.PrintWriter;

class BooleanTerm(v:Boolean, s: BooleanTermSignature) 
                                          extends PrimitiveTerm(s)
                                            with NonNumberTerm
{

  override def optValue[T](implicit mt:Manifest[T]):Option[T] =
  {
  	if (mt <:< manifest[Boolean]) {
  		Some(v.asInstanceOf[T])
  	}else if (mt <:< manifest[java.lang.Boolean]) {
  		Some(java.lang.Boolean.valueOf(v).asInstanceOf[T])
  	}else{
  		None
  	}
  }
  	
  /**
   * true	
   */
  override def isBoolean: Boolean = true;

  /**
   * get boolean value
   */
  override def getBoolean: Boolean = v;

  def fixTermEq(t:Term) = t.isBoolean && t.getBoolean == v ;

  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return c;
    if (v) {
      return (if (t.getBoolean) 0 else 1);
    } else {
      return (if (t.getBoolean) -1 else 0);
    }
  }

  def termClassIndex: Int = TermClassIndex.BOOLEAN;

  lazy val name = signature.theory.symbolTable.getOrCreate(
                                        if (v) "true" else "false" 
                                                          );

  lazy val termHashCode = name.hashCode;

  override def  print(out:PrintWriter): Unit = out.print(v);
}

object BooleanTerm
{
  def apply(v:Boolean, s: BooleanTermSignature):BooleanTerm =
                                                new BooleanTerm(v,s);
}

