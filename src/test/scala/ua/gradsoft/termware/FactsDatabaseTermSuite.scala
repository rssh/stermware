package ua.gradsoft.termware;

import org.scalatest.FunSuite;

class TestFactsDatabase extends ReflectiveFacts
{

   def f1(x:Int):Boolean =
   { return (x%3)==0; }

}

class FactsDatabaseFunSuite extends FunSuite {


  test("f1 must be viwed as condition") {
    val facts = new TestFactsDatabase(); 
    val f1Name = TermWare.instance.symbolTable.getOrCreate("f1");
    val optF1s = facts.resolveSignal(TermWare.instance,f1Name,1);
    assert(optF1s!=None);
    val fU1Name = TermWare.instance.symbolTable.getOrCreate("fU1");
    val optFU1 = facts.resolveSignal(TermWare.instance,fU1Name,1);
    assert(optFU1==None);
  }

}
