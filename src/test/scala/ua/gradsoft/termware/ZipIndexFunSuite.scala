package ua.gradsoft.termware;

import org.scalatest.FunSuite;

class SipIndexFuite extends FunSuite {

  test("1 zipindex return self") {
     val a = theory.freeAtomSignature.createConstant("a");
     val r = ZipIndex(a,BigInt(1));
     assert(r._2!=None);
     assert(r._2.get.name==a.name);
     assert(r._2.get.arity==a.arity);
  }

  test("ZipIndex over f(f1(g(0),x),f2(g2(1)),z)") {
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner("""
            f(f1(g(0),x),f2(g2(1)),z)
     """));
     val t:Term = r.get;
     val r1 = ZipIndex(t,BigInt(1*2*2));
     System.out.println("1*2*2:"+r1._2.get.name);
     assert(r1._2.get.name.getString=="g");
     val r2 = ZipIndex(t,BigInt(1*2*2+1));
     assert(r2._2.get.name.getString=="f2");
     val r3 = ZipIndex(t,BigInt(1*2*2*2+1));
     assert(r3._2.get.name.getString=="x");
     val r4 = ZipIndex(t,BigInt(1*2+1));
     assert(r4._2==None);
     val r5 = ZipIndex(t,BigInt(1*2*2+1)*2+1);
     assert(r5._2.get.name.getString=="z");
  }

  val theory = TermWare.instance.freeTheory;
  val POS = TermWare.instance.symbolTable.getOrCreate("POS");
  val parser = new TermWareParser(theory,"inside-1");


}
