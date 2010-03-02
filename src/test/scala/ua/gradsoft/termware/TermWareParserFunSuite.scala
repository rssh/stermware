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
     val theory = TermWare.instance.freeTheory;
     val POS = TermWare.instance.symbolTable.getOrCreate("POS");
     val parser = new TermWareParser(theory,"inside"); 
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner("""

       true
     """));
     //System.out.println("received:"+r);
     //System.out.println("r.class:"+r.getClass());
     r match {
       case parser.Success(t,_) => {
                       assert(t.isInstanceOf[BooleanTerm]);
                       assert(t.getBoolean.get);
                       val attr = t.getAttribute(POS).get;
                       assert(!attr.isNil);
                       assert(attr.isRef);
                       val p = attr.getRef.get.asInstanceOf[PositionWithFname];
                       System.out.println("p.line=:"+p.line);
                       assert(p.line>0);
                      }
       case _ => fail("'true' must be parsed to term");
     }
  }

  test("parse string constant with newline") {
     val theory = TermWare.instance.freeTheory;
     val POS = TermWare.instance.symbolTable.getOrCreate("POS");
     val parser = new TermWareParser(theory,"inside-1"); 
     val r = parser.phrase(parser.term)(new parser.lexical.Scanner("""
         " string with newline \"
         "
     """));
     System.out.println("received:"+r);
     r match {
       case parser.Success(t,_) => {
                       assert(t.isInstanceOf[StringTerm]);
                       val s = t.getString.get;
                       val withNL = s.contains('\n');
                       assert(withNL);
                      }
       case _ => fail("string must be parsed to string constant");
     }
  }

}
