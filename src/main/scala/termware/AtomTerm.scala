package termware

import scala.language.implicitConversions
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe.typeOf
import scala.math.ScalaNumericAnyConversions

object AtomToTerm extends ToTerm[Symbol]
{
  
   def name(x:Symbol):Name = SymbolName(x)

   def arity(x:Symbol):Int = 0

   def subterms(x:Symbol):IndexedSeq[BaseAsTerm] = IndexedSeq()

   override def subterm(x: Symbol,i:Int): Option[BaseAsTerm] = None
   
   def nameSubterms(x:Symbol):Map[Name,BaseAsTerm] = Map()

   def isAtom(x:Symbol):Boolean = true

   def isPrimitive(x:Symbol):Boolean = false

   def isNumber(x:Symbol):Boolean = false
   
   def isComplex(x:Symbol): Boolean = false

   def is[T](x:Symbol)(implicit ttag:TypeTag[T]):Boolean =
     typeOf[T] <:< typeOf[Symbol]

   def value[T](x:Symbol)(implicit ttag:TypeTag[T]):Option[T] =
     if (typeOf[T] <:< typeOf[Symbol]) {
       Some(x.asInstanceOf[T])
     } else {
       None
     }

   def numberValue(x:Symbol): Option[ScalaNumericAnyConversions] = None


   override def toTerm(x:Symbol):Term = 
                        AtomTerm(x)

   // signature part

   def  xunify(x: Symbol, y: Term, s: Substitution): UnificationResult = 
    if (y.isAtom) {
      y.name match {
        case SymbolName(n) if n == x => UnificationSuccess(s)
        case _ => UnificationFailure(x,y,s)
      }
    } else {
      UnificationFailure(x,y,s)
    }
     

   def  xsubstg(x: Symbol, s: Substitution): Term = ???

   def  xsubstv(x: Symbol, s: Substitution): Term = ???
  

}

case class AtomTerm(a:Symbol) extends Term {

  val name = SymbolName(a)
  
  def arity: Int = 0

  def subterms:IndexedSeq[Term] = IndexedSeq()

  override def subterm(i:Int): Option[Term] = None
 
  def isAtom:Boolean = true

  def isPrimitive:Boolean = false

  def isNumber: Boolean = false

  def isX : Boolean = false

  def isComplex: Boolean = false
  
  def is[T](implicit ttag:TypeTag[T]):Boolean =
    (typeOf[T] <:< typeOf[Symbol])  

  def value[T](implicit ttag:TypeTag[T]):Option[T] =
    if (typeOf[T] <:< typeOf[Symbol]) Some(a.asInstanceOf[T]) else None

  def numberValue = None

  def unify(y:Term, s: Substitution): UnificationResult =
    if (y.isAtom && name == y.name) UnificationSuccess(s) else UnificationFailure(this,y,s)

  def substg(s:Substitution): Term = s.get(this).getOrElse(this)

  def substv(s:Substitution): Term = this

  
}

object AtomTerm
{

  //  implicit def toAtom(s: Symbol) = AtomTerm(s)
  
    def unapply(s: Symbol) = AtomTerm(s)

}