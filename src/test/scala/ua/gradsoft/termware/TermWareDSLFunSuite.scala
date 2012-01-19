package ua.gradsoft.termware;

import org.scalatest._;
import org.scalatest.matchers._;
import TermWareDSL._;

class TermWareDSLFunSuite extends FunSuite 
                            //  can't use shouldmatchers, becouse they use 'a' as in our dsl
                            // with ShouldMatchers
{

  test("creation of int term") {

     val t:Term = 5;
     
     t match {
         case IntTerm(n,_) =>
                  assert(n==5);
         case _ => assert(false,"t mut be matched by int");
     }

  }

  test("creation of fun term with name") {

     val fn1:Name = theory.symbolTable.getOrCreate("fn1");

     val t1: Term = FN(fn1) with_ Seq[Term](1,2);
     val t2: Term = FN(fn1) `with` (1,2);
     val t3: Term = <>(fn1)(1,2);
     val t5: Term = <>("fn2")(1,2);

     t5 match {
        case Term(n,subterms,theory) =>
                                      assert(n.string=="fn2");
                                      assert(subterms.length==2);
                                      assert(subterms(0).isInt);
         case _ => assert(false,"t4 mut be matched by term");
     }

  }

  test("unfix operations on terms") {

     val t: Term = (FN("f")(1,2) + a("x")) * a("y");

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

  test("apply on names") {
     val t: Term = FN("f")(1,2);
     t match {
       case Term(Name("f"),Seq(x1,x2),_) =>
                 assert(x1.getInt==1,"first arg of fn must be 1");
                 assert(x2.getInt==2,"secod arg of fn must be 2");
       case _ => assert(false,"term must be matched to f");
     }
  }

  test("let expressions") {
    val t = let( x("a") <~ a("y"), x("b") <~ a("b"))(
                            FN("f")(a("a"),x("a"),x("b")) 
               );
    System.err.println("received:"+t.toString);
    t match {
      case LetTerm(bindings,body,_) =>
                 assert(body.name.string == "f" ,"body.name.string shoud equal f");
                 assert(body.subterm(0).name.string == "a", "body.subterm(0).name.string equal('a')");
                 assert(body.subterm(1).name.string == "y", "body.subterm(1).name.string == \"y\" " );
                 assert(body.subterm(2).name.string == "b", "body.subterm(2).name.string == \"b\" " );
       case _ => fail("let expression mut be matched to LetTerm");

    }
  }

  test("build rule without vars") {

     val t1:Term = a("a1");
     val t2:Term = a("a2");

     //val rule = rule(t1 -> t2);
     //System.out.println();

  }



}


