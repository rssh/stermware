package ua.gradsoft.termware;

import org.scalatest.FunSuite;
import ua.gradsoft.termware._;
import ua.gradsoft.termware.parser._;
import scala.util.parsing.combinator._;
import scala.util.parsing.combinator.lexical._;
import scala.util.parsing.input._;

class TermWareParserFunSuite extends FunSuite 
{

  test("parse boolean constant") {
     val r = parser.phrase(parser.primitiveTerm)(new parser.lexical.Scanner("""

       true
     """));
     //System.out.println("received:"+r);
     //System.out.println("r.class:"+r.getClass());
     r match {
       case parser.Success(t,_) => {
                       assert(t.isInstanceOf[BooleanTerm]);
                       assert(t.getBoolean);
                       val attr = t.getAttribute(POS).get;
                       assert(!attr.isNil);
                       assert(attr.isRef);
                       val p = attr.getRef.asInstanceOf[PositionWithFname];
                       //System.out.println("p.line=:"+p.line);
                       assert(p.line>0);
                      }
       case _ => fail("'true' must be parsed to term");
     }
  }

  test("parse string constant with newline") {
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner("""
         " string with newline \"
         "
     """));
     //System.out.println("received:"+r);
     r match {
       case parser.Success(t,_) => {
                       assert(t.isInstanceOf[StringTerm]);
                       val s = t.getString;
                       val withNL = s.contains('\n');
                       assert(withNL);
                      }
       case _ => fail("string must be parsed to string constant");
     }
  }

  test("parse int constant") {
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner("3456787"));
     r match {
       case parser.Success(t,_) => {
                       assert(t.isInstanceOf[IntTerm]);
                       val v = t.getInt;
                       assert(v==3456787);
                      }
       case _ => fail("int must be parsed to constant");
     }
  }

  test("parse functional term") {
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner("f(1,2,3)"));
     //System.err.println("received:"+r);
     r match {
       case parser.Success(t,_) => {
                       assert(t.isInstanceOf[Term]);
                       assert(t.arity==3);
                       assert(t.name.string=="f");
                       val s1 = t.subterm(0);
                       assert(s1.isInt);
                       assert(s1.getInt==1);
                       }
       case _ => fail("functional term must be parsed");
     }
  }

  test("parse complex functional term") {
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner("""
            f(f1(g(0),x),f2(g2(1)),z)
     """));
     //System.err.println("received:"+r);
     r match {
       case parser.Success(t,_) => {
                       assert(t.isInstanceOf[Term]);
                       assert(t.arity==3);
                       assert(t.name.string=="f");
                       val s1 = t.subterm(0);
                       assert(s1.arity==2);
                       assert(s1.name.string=="f1");
                       }
       case _ => fail("complex functional term must be parsed");
     }
  }

  test("parse left associative binary 1") {
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner(
        "1-1-1-1+3*4"));
     //System.err.println("received:"+r);
     r match {
       case parser.Success(t,_) => {
                       assert(t.isInstanceOf[Term]);
                       //1-1-1-1+3*4 = (((1-1)-1)-1)+(3*4))
                       assert(t.arity==2);
                       assert(t.name.string=="plus");
                       val t0 = t.subterm(0);
                       assert(t0.arity==2);
                       assert(t0.name.string=="minus");
                       val t00 = t0.subterm(0);
                       val t01 = t0.subterm(1);
                       assert(t01.isInt);
                       val t000 = t00.subterm(0);
                       val t0000 = t000.subterm(0);
                       assert(t0000.isInt);
                       val t0001 = t000.subterm(1);
                       assert(t0001.isInt);
                       val t1 = t.subterm(1);
                       assert(t1.arity==2);
       }
       case _ => fail("+ term must be parsed");
     }

  }

  test("parse left associative binary 2") {
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner(
        "3*4+6"));
     r match {
       case parser.Success(rs,_) => {
                 val t = rs.asInstanceOf[Term];
                 assert(t.name.string=="plus");
       }
       case _ => fail("this term must be parsed");
     }
  }

  test("parse with unary op 1") {
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner("-1"));
     r match {
       case parser.Success(rs,_) => {
                 val t = rs.asInstanceOf[Term];
                 assert(t.name.string=="minus");
                 assert(t.arity==1);
                        } 
       case _ => fail("this term must be parsed");
     }
  }

  test("parse new syntax definition for free right-associative") {
     val r = parser.phrase(parser.statements)(new parser.lexical.Scanner("""
       syntax: ncons binary operator "::" (assoc right, priority 10) ;
       x :: y ;
       x :: y :: z
     """));
     r match {
       case parser.Success(rs,_) => {
                 val lt = rs.asInstanceOf[List[Term]];
                 assert(lt.length==3);
                 val t1 = lt(1);
                 assert(t1.name.string=="ncons");
                 val t2 = lt(2);
                 assert(t2.name.string=="ncons");
                 val t21 = t2.subterm(1);
                 assert(t21.name.string=="ncons");
                 val t210 = t21.subterm(0);
                 assert(t210.name.string=="y");
             }
       case _ => fail("this term must be parsed");
     }
  }

  test("parse new syntax definition for list right assoc ") {
     val r = parser.phrase(parser.statements)(new parser.lexical.Scanner("""
       syntax: cons binary operator "::" (assoc right, priority 10) ;
       x :: y ;
       x :: y :: z
     """));
     r match {
       case parser.Success(rs,_) => {
                 val lt = rs.asInstanceOf[List[Term]];
                 assert(lt.length==3);
                 val t1 = lt(1);
                 assert(t1.name.string=="cons");
                 val t2 = lt(2);
                 //System.err.println("t2 is " + t2);
                 assert(t2.name.string=="cons");
                 val t21 = t2.subterm(1);
                 assert(t21.name.string=="cons");
                 val t210 = t21.subterm(0);
                 //System.err.println("t210.name " + t210.name);
                 assert(t210.name.string=="y");
             }
       case _ => fail("this term must be parsed");
     }
  }

  test("parse let expression ") {
     val r = parser.phrase(parser.statements)(new parser.lexical.Scanner("""
       let ( x <- 1, y <- 2) f(x,y)
     """));
     //System.err.println("received:"+r);
     r match {
       case parser.Success(rs,_) => {
                 val lt = rs.asInstanceOf[List[Term]];
                 val t = lt(0);
                 assert(t.name.string == "let");
                 val t0 = t.subterm(0);
                 assert(t0.name.string == "cons");
                 val t00 = t0.subterm(0);
                 assert(t00.name.string == "assign");
              }
       case _ => fail("let term must be parsed");
     }
  }


  val theory = TermWare.instance.freeTheory;
  val POS = TermWare.instance.symbolTable.getOrCreate("POS");
  val parser = new TermWareParser(theory,"inside-1"); 

}
