package ua.gradsoft.termware.test



class XVar(val i:Int, var xowner: XOwner)
{
  override def toString="v("+i+", "+xowner.toString+")";
}

class XOwner(val name: String, val vars: Seq[XVar])
{
  def this(name: String, n:Int) =
  {  this(name, XOwner.genvars1(n)); 
     for(v <- vars) v.xowner=this;
  }


  //def this(name: String, n1:Int, n2:Int) =
  //{ this(name,genvars0(n1)); }

  def genvars0(n1:Int):Seq[XVar] =  {
    for(i <- 0 until n1) yield new XVar(i, this);
  }

}

object XOwner
{

  def genvars1(n:Int): Seq[XVar] =
  {
    //System.out.println("genvars, owner="+owner);
    for(i <- 0 until n) yield new XVar(i, null);
  }


}

import org.scalatest.FunSuite;

class ScalaClassInitFunSuite extends FunSuite {

  test("xvars must be propertly initialized") {
    val xo = new XOwner("aaa",10);
    assert(xo.vars(3).xowner eq xo);
  }

}



// vim: set ts=4 sw=4 et:
