package ua.gradsoft.termware;

import org.scalatest.FunSuite;

class TermWareDSLFunSuite extends FunSuite {

  test("creation of int term") {
     import TermWareDSL._;

     val t:Term = 5;
     
     t match {
         case IntTerm(n,_) =>
                  assert(n==5);
         case _ => assert(false,"t mut be matched by int");
     }

  }

  test("creation of fun term with name") {
     import TermWareDSL._;

     val fn1:Name = theory.symbolTable.getOrCreate("fn1");

     val t1: Term = FN(fn1) with_ Seq[Term](1,2);
     val t2: Term = FN(fn1) `with` (1,2);
     val t3: Term = <>(fn1) * (1,2);
     val t4: Term = <>("fn2") * (1,2);

     t4 match {
        case Term(n,subterms,theory) =>
                                      assert(n.string=="fn2");
                                      assert(subterms.length==2);
         case _ => assert(false,"t4 mut be matched by term");
     }

  }


}
