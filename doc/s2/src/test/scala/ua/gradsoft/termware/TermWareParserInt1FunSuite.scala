package ua.gradsoft.termware;

import org.scalatest.FunSuite;
import ua.gradsoft.termware._;
import ua.gradsoft.termware.parser._;
import scala.util.parsing.combinator._;
import scala.util.parsing.combinator.lexical._;
import scala.util.parsing.input._;

class TermWareParserInt1FunSuite extends FunSuite 
{

  test("parse f1(g(0),x)") {
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner("""
            f1(g(0),x)
     """));
     //System.err.println("received:"+r);
     r match {
       case parser.Success(t,_) => {
                       assert(t.isInstanceOf[Term]);
                       assert(t.arity==2);
                       assert(t.name.string=="f1");
                       val s1 = t.subterm(0);
                       assert(s1.arity==1);
                       assert(s1.name.string=="g");
                       }
       case _ => fail("f1(g(0),x) must be parsed");
     }
  }

  test("parse g(0)") {
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner("""
            g(0)
     """));
     //System.err.println("received:"+r);
     r match {
       case parser.Success(t,_) => {
                       assert(t.isInstanceOf[Term]);
                       assert(t.arity==1);
                       assert(t.name.string=="g");
                       }
       case _ => fail("g(0) must be parsed");
     }
  }

  test("parse 0 as term") {
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner("""
            0
     """));
  //   System.err.println("received:"+r);
     r match {
       case parser.Success(t,_) => {
                       assert(t.isInstanceOf[Term]);
                       assert(t.isInt);
                       }
       case _ => fail("0 must be parsed");
     }
  }

  test("parse 1 as term") {
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner("""
            1
     """));
     r match {
       case parser.Success(t,_) => {
                       assert(t.isInstanceOf[Term]);
                       assert(t.isInt);
                       }
       case _ => fail("1 must be parsed");
     }
  }


  val theory = TermWare.instance.freeTheory;
  val POS = TermWare.instance.symbolTable.getOrCreate("POS");
  val parser = new TermWareParser(theory,"inside-1"); 

}
