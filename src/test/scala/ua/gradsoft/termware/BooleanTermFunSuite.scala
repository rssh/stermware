package ua.gradsoft.termware;

import org.scalatest.FunSuite;

class BooleanTermFunSuite extends FunSuite {

  test("boolean term must be isBoolean") {
     val t = TermWare.instance.booleanSignature.createConstant(false);
     var b = t.isBoolean;
     assert(t.isBoolean);
  }

  test("boolean term must unificated with self") {
     val t = TermWare.instance.booleanSignature.createConstant(false);
     val optS = t.termUnify(t,SimpleSubstitution.empty);
     assert(optS!=None);
  }

  test("true and false term must not be unificated") {
     val t = TermWare.instance.booleanSignature.createConstant(true);
     val f = TermWare.instance.booleanSignature.createConstant(false);
     val optS = t.termUnify(f,SimpleSubstitution.empty);
     assert(!optS._1);
  }

}
