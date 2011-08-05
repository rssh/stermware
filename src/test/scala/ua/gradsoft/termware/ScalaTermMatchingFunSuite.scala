package ua.gradsoft.termware;

import org.scalatest.FunSuite;

class ScalaTermMatchingFunSuite extends FunSuite {

  test("func term musst be matched as Term") {
     val it = TermWare.instance.intSignature.createConstant(1);
     val st = TermWare.instance.stringSignature.createConstant("sss");
     val fn = TermWare.instance.symbolTable.getOrCreate("f");
     val ct = TermWare.instance.funSignature(fn).createTerm(fn,IndexedSeq(it,st));
     
     ct match {
         case Term(Name("f"),subterms,_) =>
                  assert(subterms.size==2);
                  assert(subterms(0).isInt);
                  assert(subterms(1).isString);
         case _ => assert(false,"term must be matched to term expression");
     }

     ct match {
         case Term(Name("f"),Seq(x,y) ,_) =>
                  assert(x.isInt);
                  assert(y.isString);
         case _ => assert(false,"term must be matched to term expression");
     }


  }

  test("string term musst be matched as StringTerm") {
     val st = TermWare.instance.stringSignature.createConstant("sss");
     st match {
       case StringTerm(s,_) => assert(s=="sss");
       case _ => assert(false,"string term must be matched to StringTerm expression");
     }
     st match {
       case Term(Name("sss"),Seq(),_) => /* nothing */
       case _ => assert(false,"string term must be matched to Term expression");
     }
  }

  test("IntTerm must be matched with name IntName") {
     val it = TermWare.instance.intSignature.createConstant(1);
     it match {
         case Term(IntName(1),_,_) => /* ok */
         case _ => assert(false,"int term must be matched to Term expression");
     }
  }

}
