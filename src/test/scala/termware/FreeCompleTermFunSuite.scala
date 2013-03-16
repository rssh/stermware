package termware

import org.scalatest.FunSuite

class FreeCompleTermFunSuite extends FunSuite
  // note, that ShoultMatchers conflicts with free term constructor
{

  test("free complex term") {
    val t: Term = 'f(1,'g(3),3,"AAA")
    assert(t.arity==4)
    val f0 = t.subterms(0)
    assert(f0.is[Int]) 
    val f1 = t.subterms(1)
    assert(f1.isComplex)
    assert(f1.arity==1)
  /*    
    t match {
      case Term('f,args) => args(0) should be equal(f0)
    }
  */

  }


}


// vim: set ts=4 sw=4 et:
