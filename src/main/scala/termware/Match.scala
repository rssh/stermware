package termware

sealed trait Match
{
  def put(x:TermWithContext ,y:TermWithContext, u: Unification, s: Term): Match
                     // ts ?
  def merge(x: Match, u: Unification, s:Term): Match

}


case class Substitution(mapping: Map[TermWithContext,TermWithContext]) extends Match
{

  def put(x:TermWithContext, y:TermWithContext, u: Unification, s: Term): Match =
  {
    def updated = Substitution(mapping.updated(x,y))
    mapping.get(x) match {
     case Some(z) => u(y,z,s) match {
                       case yz@Substitution(_) => updated.merge(yz,u,s)
                       case f@MatchFailure(_,_,_) => f
                     }
     case None => updated
    }
  }

  def merge(x: Match, u: Unification, scope: Term): Match =
    x match {
      case Substitution(xm) => 
             val s0: Match = this
             xm.foldLeft(s0){ (s,e) => s.put(e._1,e._2,u,scope) }
      case f@MatchFailure(_,_,_) => f
    }

}

case class MatchFailure(val x: TermWithContext, val y: TermWithContext, val reason: String) extends Match
{

  def put(x:TermWithContext,y:TermWithContext, u: Unification, s: Term): Match = this

  def merge(x: Match, u: Unification, s:Term ): Match = this

}

object Substitution
{
  val empty = Substitution(Map())
}

