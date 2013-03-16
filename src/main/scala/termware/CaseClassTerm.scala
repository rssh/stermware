package termware

import scala.reflect._
import scala.reflect.runtime.universe._

class CaseClassToTerm[X <: AnyRef with Product](implicit xtag:TypeTag[X]) extends ComplexToTerm[X]
{

  lazy val fixedName = SymbolName(Symbol(xtag.tpe.typeSymbol.asClass.name.toString()))
  
  def name(x:X) = fixedName
  
   // Members declared in termware.ComplexToTerm
  def arity(x: X): Int = x.productArity
  
  def nameSubterms(x: X): Map[Name, Term] = 
    fixedNameSubterms

  def fixedNameSubterms: Map[Name, Term] = ???
    
  def subterms(x: X): IndexedSeq[Term] = 
    Reflect.caseClassSubterms(x,xtag)
    
  def isComplex(x:X): Boolean = x.productArity > 0   

  def isNumber(x: X): Boolean = false
  
  def numberValue(x: X): Option[scala.math.ScalaNumericAnyConversions] = None
  
  def xsubstg(x: X,s: termware.Substitution): termware.Term = ???
  
  def xsubstv(x: X,s: termware.Substitution): termware.Term = {
    new AsTerm(x,this)
  }
 
  def xunify(x: X,y: termware.Term,s: termware.Substitution): termware.UnificationResult = ???
  
  
}




