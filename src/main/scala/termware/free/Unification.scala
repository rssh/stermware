package termware.free

import termware._

object Unification extends termware.Unification
{

  // naive recursive implementation.
  // TODO: rewrite
  def apply(x: Term, y: Term, scope: Term): Match =
  {
    x match {
      case AtomTerm(xv,xa,xts) =>
        y match {
           case AtomTerm(yv,ya,yts) =>
             if (xv == yv) {
                Substitution.empty
             } else {
                MatchFailure(x,y,"different atoms")
             }
           case VarTerm(yn,yi,ys,xa,ts) =>
             if (ys.forall(_ eq scope)) {
                Substitution(Map(y->x))
             } else {
                MatchFailure(x,y,"can't unify atom and non-with var")
             }
           case _ =>
                MatchFailure(x,y,"can't unify atom with non-atom")
        }
      case px:PrimitiveTerm =>
        y match {
           case py:PrimitiveTerm => applyPrimitive(px,py)
           case VarTerm(yn,yi,ys,xa,ts) =>
             if (ys.forall(_ eq scope)) {
                Substitution(Map(y->x))
             } else {
	        MatchFailure(x,y,"can't unify primitive and non-with var")
             }
           case _ =>
                MatchFailure(x,y,"can't unify primitive with non-primitive")
        }
      case StructuredTerm(xstructure,xcomponents,xa,xts) =>
         y match {
           case StructuredTerm(ystructure,ycomponents,ya,yts) =>
             if (ystructure.name != ystructure.name) {
                MatchFailure(x,y,"name mismatch")
             } else if (xcomponents.length != ycomponents.length) {
                MatchFailure(x,y,"arity mismatch")
             } else {
                var s0: Match = Substitution.empty           
                (xcomponents zip ycomponents).foldLeft(s0){ (s,e) => 
                                 s.merge(apply(e._1,e._2,scope),this,scope)  }
             }
           case VarTerm(yn,yi,ys,xa,ts) =>
             if (ys.forall(_ eq scope)) {
                Substitution(Map(y->x))
             } else {
	        MatchFailure(x,y,"can't unify structured term and non-with var")
             }
           case _ =>
                MatchFailure(x,y,"structure mismatch")
         }
      case VarTerm(xn,xi,xs,xa,ts) =>
         if (xs.forall(_ eq scope)) {
           Substitution(Map(x->y))
         } else {
           y match {
             case VarTerm(yn,yi,ys,xa,ts) =>
               if (ys.forall(_ eq scope)) {
                 Substitution(Map(y->x))
               } else if (ys.get eq xs.get) {
                 Substitution.empty
               } else {
                 MatchFailure(x,y,"var mismatch")
               }
             case _ =>
                 MatchFailure(x,y,"var mismatch")
           }
         }
    }
  }

  def applyPrimitive(x: PrimitiveTerm, y: PrimitiveTerm): Match =
    x match {
      case StringTerm(xv,xa,xts) =>
             y match {
               case StringTerm(yv,ya,yts) =>
                      if (xv == yv) Substitution.empty 
                        else MatchFailure(x,y,"string mismatch")
               case _ =>
                      MatchFailure(x,y,"type mismatch")
             }
      case CharTerm(xv,xa,xts) =>
             y match {
               case CharTerm(yv,ya,yts) =>
                      if (xv == yv) Substitution.empty 
                        else MatchFailure(x,y,"char mismatch")
               case _ =>
                      MatchFailure(x,y,"type mismatch")
             }
      case Int32Term(xv,xa,xts) =>
             y match {
               case Int32Term(yv,ya,yts) =>
                      if (xv == yv) Substitution.empty 
                        else MatchFailure(x,y,"int32 mismatch")
               case _ =>
                      MatchFailure(x,y,"type mismatch")
             }
      case Int64Term(xv,xa,xts) =>
             y match {
               case Int64Term(yv,ya,yts) =>
                      if (xv == yv) Substitution.empty 
                        else MatchFailure(x,y,"int64 mismatch")
               case _ =>
                      MatchFailure(x,y,"type mismatch")
             }
      case DoubleTerm(xv,xa,xts) =>
             y match {
               case DoubleTerm(yv,ya,yts) =>
                      if (xv == yv) Substitution.empty 
                        else MatchFailure(x,y,"double mismatch")
               case _ =>
                      MatchFailure(x,y,"type mismatch")
             }
      case OpaqueTerm(xv,xa,xts) =>
             y match {
               case OpaqueTerm(yv,ya,yts) =>
                      if (xv == yv) Substitution.empty 
                        else MatchFailure(x,y,"double mismatch")
               case _ =>
                      MatchFailure(x,y,"type mismatch")
             }
    }

}
