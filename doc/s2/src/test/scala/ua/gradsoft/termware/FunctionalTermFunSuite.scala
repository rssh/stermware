package ua.gradsoft.termware;

import org.scalatest.FunSuite;
import org.scalatest.matchers.ShouldMatchers;

class FunctionalTermFunSuite extends FunSuite 
                         with ShouldMatchers
{

  test("functional terms must be able to created") {
    val r = parser.phrase(parser.term)(new parser.lexical.Scanner("g(x,x,x)"));
    r match {
       case parser.Success(t,_) => 
           assert(t.arity==3,"resut term must have arity 3, as g(x,x,x)");
           t match {
             case FunctionalTerm(name, Seq(p1,p2,p3), _ ) =>
                             assert(name.string=="g");
             case _ => fail("expression must be matched as functional expression, if functional expression inside");
           }
       case _ => fail("functional term must be parsed");
    }
  }

  test("substitution must works for simple functional expressions") {
    val r = parser.phrase(parser.term)(new parser.lexical.Scanner("""
                 f(x,g(x,y),y)
            """));
    val t = r match {
       case parser.Success(t,_) => t
       case _ => fail("functional term must be parded");
    }
    val z = theory.freeAtomSignature.createConstant("z");
    object s1 extends PartialFunction[Term,Term] {
      def isDefinedAt(x:Term):Boolean = x.isAtom && x.name.string == "y";
      def apply(x:Term):Term = z ;       
    }
    val t1 = t.fixSubst(s1)
    assert(t.arity == t1.arity);
    t1 match {
       case Term(Name("f"),Seq(a1,a2,a3),_) =>
              a1.name.string should equal("x")
              a3.name.string should equal("z")
              a2.name.string should equal("g")
              a2.subterms(0).name.string should equal("x")
              a2.subterms(1).name.string should equal("z")
       case _ => fail("substitutd term must be matched");
    }
  }


  val theory = TermWare.instance.freeTheory;
  val parser = new TermWareParser(theory,"inside-FunctionalTermFunSuite");

}
