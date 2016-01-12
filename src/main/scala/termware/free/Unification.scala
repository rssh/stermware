package termware.free

import termware._

object Unification extends termware.Unification
{

  // naive recursive implementation.
  // TODO: rewrite
  def apply(x: Term, y: Term, scope: Term): Match =
  {
    x match {
      case AtomTerm(xv,xa) =>
        y match {
           case AtomTerm(yv,ya) =>
             if (xv == yv) {
                Substitution.empty
             } else {
                MatchFailure(x,y,"different atoms")
             }
           case VarTerm(yn,yi,ys,xa) =>
             if (y.scopeIndex == scope.scopeIndex) {
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
           case VarTerm(yn,yi,ys,xa) =>
             if (y.scopeIndex == scope.scopeIndex) {
                Substitution(Map(y->x))
             } else {
	        MatchFailure(x,y,"can't unify primitive and non-with var")
             }
           case _ =>
                MatchFailure(x,y,"can't unify primitive with non-primitive")
        }
      case StructuredTerm(xstructure,xcomponents,xa) =>
         y match {
           case StructuredTerm(ystructure,ycomponents,ya) =>
             if (ystructure.name != ystructure.name) {
                MatchFailure(x,y,"name mismatch")
             } else if (xcomponents.length != ycomponents.length) {
                MatchFailure(x,y,"arity mismatch")
             } else {
                var s0: Match = Substitution.empty           
                // TODO: define components as term-with-context.
                (xcomponents zip ycomponents).foldLeft(s0){ (s,e) => 
                                 s.merge(apply(e._1,e._2,scope),this,scope) }
             }
           case VarTerm(yn,yi,ys,xa) =>
             if (y.scopeIndex == scope.scopeIndex) {
                Substitution(Map(y->x))
             } else {
	        MatchFailure(x,y,"can't unify structured term and non-with var")
             }
           case _ =>
                MatchFailure(x,y,"structure mismatch")
         }
      case VarTerm(xn,xi,xs,xa) =>
         if (x.scopeIndex == scope.scopeIndex) {
           Substitution(Map(x->y))
         } else {
           y match {
             case VarTerm(yn,yi,ys,xa) =>
               if (y.scopeIndex == scope.scopeIndex) {
                 Substitution(Map(y->x))
               } else if ( y.scopeIndex == x.scopeIndex && xi == yi) {
                 // TODO: review.
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
      case StringTerm(xv,xa) =>
             y match {
               case StringTerm(yv,ya) =>
                      if (xv == yv) Substitution.empty 
                        else MatchFailure(x,y,"string mismatch")
               case _ =>
                      MatchFailure(x,y,"type mismatch")
             }
      case CharTerm(xv,xa) =>
             y match {
               case CharTerm(yv,ya) =>
                      if (xv == yv) Substitution.empty 
                        else MatchFailure(x,y,"char mismatch")
               case _ =>
                      MatchFailure(x,y,"type mismatch")
             }
      case Int32Term(xv,xa) =>
             y match {
               case Int32Term(yv,ya) =>
                      if (xv == yv) Substitution.empty 
                        else MatchFailure(x,y,"int32 mismatch")
               case _ =>
                      MatchFailure(x,y,"type mismatch")
             }
      case Int64Term(xv,xa) =>
             y match {
               case Int64Term(yv,ya) =>
                      if (xv == yv) Substitution.empty 
                        else MatchFailure(x,y,"int64 mismatch")
               case _ =>
                      MatchFailure(x,y,"type mismatch")
             }
      case DoubleTerm(xv,xa) =>
             y match {
               case DoubleTerm(yv,ya) =>
                      if (xv == yv) Substitution.empty 
                        else MatchFailure(x,y,"double mismatch")
               case _ =>
                      MatchFailure(x,y,"type mismatch")
             }
      case OpaqueTerm(xv,xa) =>
             y match {
               case OpaqueTerm(yv,ya) =>
                      if (xv == yv) Substitution.empty 
                        else MatchFailure(x,y,"double mismatch")
               case _ =>
                      MatchFailure(x,y,"type mismatch")
             }
    }

}

