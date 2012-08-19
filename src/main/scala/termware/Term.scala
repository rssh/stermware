package termware

import scala.reflect.runtime.universe._

trait Term
{

  def name: Name

  def arity: Int

  def subterms:IndexedSeq[Term]

  def subterm(i:Int): Option[Term] = 
       if (i<arity) Some(subterms(i)) else None 

}


/**
 * typeclass for term algebra.
 */
trait ToTerm[T]
{

   type Origin = T

   def name(t:T):Name

   def arity(t:T):Int

   def subterms(t:T):IndexedSeq[BaseAsTerm]

   def subterm(t:T,i:Int): Option[BaseAsTerm] = 
          if (i<arity(t)) 
               Some(subterms(t)(i))
          else
               None
   
   def nameSubterms(t:T):Map[Name,BaseAsTerm]

   def nameSubterm(t:T,n:Name): Option[BaseAsTerm] =
               nameSubterms(t).get(n)

   def isAtom(t:T):Boolean;

   def is[X](t:T)(implicit ttag:TypeTag[T], xtag:TypeTag[X]):Boolean

   def value[X](t:T)(implicit ttag:TypeTag[T], xtag:TypeTag[X]):Option[X]

   def isX(t:T):Boolean = false

   def xNum(t:T):Long = 0L

   def toTerm(t:T):Term = 
                new AsTerm(t,this)

   implicit def toTerm:ToTerm[T] = this;

}

trait BaseAsTerm extends Term
{
  type ValueType;
  type WrapperType <: ToTerm[ValueType];
}

class AsTerm[T](x:T, tt: ToTerm[T]) extends BaseAsTerm 
{

   type ValueType = T;

   def name:Name = tt.name(x)
         
   def arity:Int = tt.arity(x)

   def subterms: IndexedSeq[Term] = 
                            tt.subterms(x)

   def nameSubterms:Map[Name,Term] = 
                            tt.nameSubterms(x)

}



object IntToTerm extends ToTerm[Int]
{

   def name(t:Int):Name = PrimitiveName[Int](t)

   def arity(t:Int) = 0

   def subterms(t:Int):IndexedSeq[BaseAsTerm] = IndexedSeq()

   def nameSubterms(t:Int):Map[Name,BaseAsTerm] = Map()

   def isAtom(t:Int) = false

   def is[X](t:Int)(implicit ttag:TypeTag[Int], xtag:TypeTag[X]):Boolean =
                               (typeOf[X] <:< typeOf[Int]) 
     
   def value[X](t:Int)(implicit ttag:TypeTag[Int], xtag:TypeTag[X]):Option[X] =
      if (typeOf[X] <:< typeOf[Int]) {
         Some(t.asInstanceOf[X])
      } else {
         None
      }


}


// vim: set ts=4 sw=4 et:
