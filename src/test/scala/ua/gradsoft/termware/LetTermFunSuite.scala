package ua.gradsoft.termware;

import org.scalatest.FunSuite;

class LetTermFunSuite extends FunSuite 
                         //with ShouldMatches
{

  test("let term must be able to be created from assignmetns and main term") {
    val r = parser.phrase(parser.term)(new parser.lexical.Scanner("let (x<-f(1)) g(x,x,x)"));
    r match {
       case parser.Success(t,_) => {
                         t match {
                           case FunctionalTerm(theory.Let,
                                               Seq(assignments, body), _) =>
                                  // ok. now we have functional let term.
                                  // let's transform one to let term.
                                  val letTerm = LetTerm.build(assignments,body,theory);
                                  //assert(letTerm.vars.length==1);
                                  //assert(letTerm.vars(0).name.string=="x");
                                  ////assert(letTerm.proxy.name.string=="g");
                                  ////assert(letTerm.proxy.arity==3);
                                  assert(letTerm.arity==3,"resut term must have arity 3, as g(x,x,x)");
                                  assert(letTerm.name.string=="g", "and name of internal term" );
                                  val G = theory.symbolTable.getOrCreate("g");
                                  letTerm match {
                                    case FunctionalTerm(G,Seq(x1,x2,x3),_) =>
                                         //x1.name.string should be ("f");
                                         assert(x1.name.string=="f");
                                         //x1.arity should be (1);
                                         assert(x1.arity==1);
                                    case _ => fail("let expression must be matched as functional expression, if functional expression inside");

                                  }
                                  letTerm match {
                                    case LetTerm(xbindings,xInternalTerm,_) =>
                                           /* ok */
                                    case _ => fail("let expression must be matched as LetTern");
                                  }

                           case _ => fail("let-expresion must be parsed as functional term");
                         }
                       }
       case _ => fail("let term must be parsed");
    }
  }

  test("internal variable in internal let must hide one in external let") {
    pending;
  }

  test("substitution on let must be anoter let") {
    pending;
  }

  test("substitution on let must double internal let's") {
    pending;
  }

  val theory = TermWare.instance.freeTheory;
  val parser = new TermWareParser(theory,"inside-LetTermFunSuite");


}
