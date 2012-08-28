package termware

object TermOrdering extends Ordering[Term]
{

  //TODO: optimize, check one level before call trampoline.
  def compare(x:Term, y:Term):Int =
    cbCompare(x,y).get
   
  def cbCompare(x:Term, y:Term):ComputationBounds[Int] =
  {
     (x.arity - y.arity) match {
        case c if c!=0 => Done(c)
        case _ => 
          (x.name compare y.name) match {
            case c if c!=0 => Done(c)
            case _ => cbCompare(x.subterms, y.subterms)
          }
     }
  }

  def cbCompare(x:IndexedSeq[Term], y:IndexedSeq[Term]):ComputationBounds[Int]=
   { 
     if (x.isEmpty)  
       Done(if (y.isEmpty) 0 else -1)
     else if (y.isEmpty) 
       Done(1)
     else
       cbCompare(x.head, y.head) flatMap { r =>
         if (r!=0) Done(r) else cbCompare(x.tail, y.tail)
       }
   }

}

