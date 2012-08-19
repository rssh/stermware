package termware


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class ComputationBoundsFunSuite extends FunSuite
                                    with ShouldMatchers
{

  test("100000 recursive calls must be handled") {
    def f1(n:Int): ComputationBounds[Int] = Call{ (tid,depth) =>
      //System.err.println("f1(%d), nesting=%s".format(n,depth.toString));
      if (n==0) Done(0)
      else if (n>0) {
          Call.nested(tid,depth){ (tid, depth) =>
            //System.err.println("in nested, depth="+depth.toString);
            for(x <- f1(n-1)) yield x+n ;
          }
      } else {
          throw new IllegalArgumentException("n must be >0");
      }
    }
    def f2(n:Int):Int = (1 to n).sum
    try {
     val x1 = f1(100000).get
     val x2 = f2(100000)
     x1 should equal(x2)
    } catch {
      case ex: StackOverflowError => ex.printStackTrace();
                                     throw new RuntimeException(ex); 
    }
  }

  test("internal rec call must be work") {
    def f1(n:Int): ComputationBounds[Int] = Call{ (tid,depth) =>
      //System.err.println("f1(%d), depth=%d".format(n,depth));
      if (n==0) Done(0)
      else if (n>0) {
        for(x <- f1(n-1)) yield x+n ;
      } else {
         throw new IllegalArgumentException("n must be >0");
      }
    }
    def f2(n:Int):Int = (1 to n).sum
    val x1 = f1(100000).get
    val x2 = f2(100000)
    x1 should equal(x2)
  }

  test("inProcess shoud show depth and trampolineId") {
     var maxDepth: Int = 0;
     def f(n:Int):ComputationBounds[Int] = Call{ (tid, depth) =>
        System.err.println("f1(%d), depth=%d".format(n,depth));
        if (n > 1) {
          for(x <- f(n-1).inProcess) yield {
            (tid,depth) => System.err.println("tid="+tid+", depth="+depth)
            if (n > maxDepth) {
              maxDepth = n
            }
            n + depth
          }
        } else Done(1)
     }
     val x = f(10).get
     maxDepth should be >(0)
  }


}


// vim: set ts=4 sw=4 et:
