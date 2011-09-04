package ua.gradsoft.termware;

import java.io.PrintWriter;

case class CharName(v:Char) extends Name
{
 def kindIndex: Int = NameKindIndex.CHAR.id;
 def index: Int = 0;
 def string: String = v.toString;

 override def compare(that: Name):Int =
   if (kindIndex == that.kindIndex)
        string.compareTo(that.string);
   else
        kindIndex - that.kindIndex
   ;

}

class CharTerm(v:Char, s: CharTermSignature) extends PrimitiveTerm(s)
                                                  with NonNumberTerm
                                                  with NonBooleanTerm
{

  override def isChar: Boolean = true;
  override def getChar: Char = v;

  def fixTermEq(t:Term):Boolean = t.isChar && t.getChar == v;

  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    return v.compare(t.getChar);
  }

  def termClassIndex: Int = TermClassIndex.CHAR;

  lazy val name = new CharName(v);
  lazy val termHashCode = v.hashCode;

  override def print(out:PrintWriter):Unit = { out.print(v); }

}

object CharTerm
{
   def apply(v:Char, s: CharTermSignature):CharTerm = new CharTerm(v,s);
}

