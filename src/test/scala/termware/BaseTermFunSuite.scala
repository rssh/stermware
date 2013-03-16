package termware


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class BaseTermFunSuite extends FunSuite
                                    with ShouldMatchers
{

  test("int term must exists") {
    val t: Term = 5
    t.arity should be(0)
    val subterms = 5.subterms    
    t.is[Int] should be(true)
    t.value[Int] should be(Some(5))
    t.isNumber should be(true)
  }

  test("long term must exists") {
    val t: Term = 5L
    t.arity should be(0)
    t.is[Long] should be(true)
    t.is[Int] should be(false)
    t.value[Long] should be(Some(5L))
    t.isNumber should be(true)
    t.numberValue.get.intValue() should be(5)
  }
  
  test("string term") {
    val t: Term = "AAA"
    t.arity should be(0)
    t.is[String] should be(true)
    t.value[String] should be(Some("AAA"))
  }

  test("atom term") {
    val t:Term = 'a
    t.arity should be(0)
    t.isAtom should be(true)
  }
  

}


// vim: set ts=4 sw=4 et:
