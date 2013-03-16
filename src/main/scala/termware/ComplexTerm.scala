package termware

import scala.reflect.runtime.universe._
import scala.math._

trait ComplexTerm extends Term
{

  def isAtom:Boolean = false

  def isPrimitive:Boolean = false

  def isNumber: Boolean = false
  
  def isComplex: Boolean = true

  // withoyut default mapping to laguage objects.
  def is[T](implicit ttag:TypeTag[T]):Boolean = false

  def value[T](implicit ttag:TypeTag[T]):Option[T] = None
    
  def numberValue: Option[ScalaNumericAnyConversions] = None

  def isX : Boolean = false

  
  
}


abstract class ComplexToTerm[X](implicit xtag: TypeTag[X]) extends ToTerm[X]
{

   def name(x:X):Name

   def arity(x:X):Int

   def subterms(x:X):IndexedSeq[Term]

   def nameSubterms(x:X):Map[Name,Term] 

   def isAtom(x:X) = false

   def isPrimitive(x:X) = false

   def is[T](x:X)(implicit ttag:TypeTag[T]):Boolean =
                               (typeOf[T] <:< typeOf[X]) 
     
   def value[T](t:X)(implicit ttag:TypeTag[T]):Option[T] =
      if (typeOf[T] <:< typeOf[X]) {
         Some(t.asInstanceOf[T])
      } else {
         None
      }

   type VType = X

   def vTag: TypeTag[X] = xtag

   def v(x:X):X = x


}

// vim: set ts=4 sw=4 et:
