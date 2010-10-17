package ua.gradsoft.termware;

import java.io.PrintWriter;

case class AtomTerm(val name:Name,s:AtomTermSignature) extends PrimitiveTerm(s)
                                            with NonNumberTerm
{

  override def isAtom = true;

  override def isNil = false;
  
  def termClassIndex=TermClassIndex.ATOM;

  def termCompare(t:Term):Int = {
    val cl = termClassIndex - t.termClassIndex;
    if (cl != 0) return cl;
    return name.compareTo(t.name); 
  }

  def termHashCode: Int = 1+name.hashCode;

  override def toString = name.toString;

  override def print(out:PrintWriter): Unit = out.print(name.string); 

}

