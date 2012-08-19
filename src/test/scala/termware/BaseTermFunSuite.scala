package termware


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class BaseTermFunSuite extends FunSuite
                                    with ShouldMatchers
{

  test("int term must exists and have arity 0") {
    val t: Term = 5
    t.arity should be(0)
    val subterms = 5.subterms    
    t.is[Int] should be(true)
  }



}


// vim: set ts=4 sw=4 et:
