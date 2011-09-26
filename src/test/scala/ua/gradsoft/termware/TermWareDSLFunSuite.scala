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
                                      assert(subterms(0).isInt);
         case _ => assert(false,"t4 mut be matched by term");
     }

  }

  test("unfix operations on terms") {
     import TermWareDSL._;

     val t: Term = (FN("f")*(1,2) + a("x")) * a("y");

     t match {
       case Term(Name("multiply"),Seq(x1,x2),_) =>
              x1 match {
                case Term(Name("plus"),Seq(x11,x12),_) =>
                              assert(x11.name.string == "f");
                              assert(x11.subterm(0).isInt);
                case _ => assert(false,"first argument must be plus expr");
              }
              assert(x2.isAtom);
       case _ => assert(false,"term must be matched to multiply");
     }

  }

  test("build rule without vars") {
     import TermWareDSL._;

     val t1:Term = a("a1");
     val t2:Term = a("a2");

     //val rule = rule(t1 -> t2);
     //System.out.println();

  }



}


