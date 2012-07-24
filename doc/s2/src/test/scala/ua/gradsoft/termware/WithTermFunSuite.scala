package ua.gradsoft.termware;

import org.scalatest.FunSuite;
import org.scalatest.matchers.ShouldMatchers;

class WithTermFunSuite extends FunSuite 
                         with ShouldMatchers
{

  test("with term must be able to be created from ") {
    val r = parser.phrase(parser.term)(new parser.lexical.Scanner(
              "with(x,y,z) g(x,y,z)"
    ));
    r match {
       case parser.Success(t,_) => {
                         t match {
                           case FunctionalTerm(theory.With,
                                               Seq(vars, body), _) =>
                                  // ok. now we have functional with term.
                                  // let's transform one to with term.
                                  val withTerm = WithTerm.build(vars,body,theory);
                                  assert(withTerm.arity==3,"resut term must have arity 3, as g(x,x,x)");
                                  assert(withTerm.name.string=="g", "and name of internal term" );
                                  val G = theory.symbolTable.getOrCreate("g");
                                  withTerm match {
                                    case FunctionalTerm(G,Seq(x1,x2,x3),_) =>
                                         //x1.name.string should be ("f");
                                         assert(x1.isX);
                                         assert(x2.isX);
                                         assert(x3.isX);
                                    case _ => fail("with expression must be matched as functional expression, if functional expression inside");

                                  }
                                  withTerm match {
                                    case WithTerm(xbindings,xInternalTerm,_) =>
                                           /* ok */
                                    case _ => fail("with expression must be matched as WithTerm");
                                  }

                           case _ => fail("with-expresion must be parsed as functional term");
                         }
                       }
       case x => fail("with term must be parsed:"+x.toString);
    }
  }

  test("internal variable in internal let must hide one in external with") {
    pending
  }

  test("substitution on with must be anoter with") {
    pending
  }

  test("substitution on with must change atoms in internal with's") {
    pending
  }

  test("substitution on with term when all vars bound must return non-with term") {
    pending
  }

  val theory = TermWare.instance.freeTheory;
  val parser = new TermWareParser(theory,"inside-WithTermFunSuite");


}
