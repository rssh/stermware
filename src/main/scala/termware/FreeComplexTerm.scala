package termware

import scala.math._
import scala.reflect.runtime.universe._

case class FreeComplexTerm(val name: Name, 
                           val subterms: IndexedSeq[Term]
                          )extends ComplexTerm {
  

  def arity: Int = subterms.length


  /**
   * unify self with term <code> y </code>
   */
  // TODO: add nesting parameter and swith to computatio bounds if one is big.
  def unify(y:Term, s0: Substitution): UnificationResult =
    if (y.arity != arity) 
      UnificationFailure(this,y,s0)
    else {
      var quit = false
      var n = arity
      var i =0
      var s = s0
      var r: UnificationResult = UnificationSuccess(s)
      while( i < arity && !quit) {
         val cx = subterm(i).get
         val cy = y.subterm(i).get
         i += 1
         r = cx.unify(cy,s0)
         r match {
           case UnificationSuccess(substitution) =>
             s = substitution
           case UnificationFailure(x,y,substitution) =>
             quit = true
         }
      }
      r
    }

  
      
  /**
   * generic substitution, where in right part can be any term
   */
  def substg(s:Substitution): Term =
    FreeComplexTerm(name,subterms map (_.substg(s)))

  /**
   * substitution where in right part can be only variable definitions.
   */
  def substv(s: Substitution): Term =
   FreeComplexTerm(name, subterms map (_.substv(s)))
 

}