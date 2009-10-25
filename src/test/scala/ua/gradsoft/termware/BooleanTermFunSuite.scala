package ua.gradsoft.termware;

import org.scalatest.FunSuite;

class BooleanTermFunSuite extends FunSuite {

  test("boolean term must be isBoolean") {
     val t = new BooleanTerm(false);
     assert(t.isBoolean);
  }

  test("boolean term must unificated with self") {
     val t = new BooleanTerm(false);
     val optS = t.termUnify(t,Substitution.empty);
     assert(optS!=None);
  }

  test("next test will be pendign") (pending)
}
