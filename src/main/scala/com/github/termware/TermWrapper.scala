package com.github.termware

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
trait TermWrapper[T]
{

   type TType = T

   def name(t:T):Name

   def arity(t:T):Int

   def subterms(t:T):IndexedSeq[BaseWrap]

   def subterm(t:T,i:Int): Option[BaseWrap] = 
          if (i<arity(t)) 
               Some(subterms(t)(i))
          else
               None
   
   def nameSubterms(t:T):Map[Name,BaseWrap]

   def nameSubterm(t:T,n:Name): Option[BaseWrap] =
               nameSubterms(t).get(n)

   def isAtom(t:T):Boolean;

   def is[X](t:T)(implicit ttag:TypeTag[T], xtag:TypeTag[X]):Boolean

   def value[X](t:T)(implicit mt:Manifest[T], mx:Manifest[X]):Option[X]

   def isX(t:T):Boolean = false

   def xNum(t:T):Long = 0L

   def toTerm(t:T):Term = 
                new WrappedTerm(t,this)

   implicit def termWrapper:TermWrapper[T] = this;

}

trait BaseWrap extends Term
{
  type ValueType;
  type WrapperType <: TermWrapper[ValueType];
}

class WrappedTerm[T](x:T, tw: TermWrapper[T]) extends BaseWrap 
{

   type ValueType = T;

   def name:Name = tw.name(x)
         
   def arity:Int = tw.arity(x)

   def subterms: IndexedSeq[Term] = 
                            tw.subterms(x)

   def nameSubterms:Map[Name,Term] = 
                            tw.nameSubterms(x)

}



object IntTermWrapper extends TermWrapper[Int]
{

   def name(t:Int):Name = PrimitiveName[Int](t)

   def arity(t:Int) = 0

   def subterms(t:Int):IndexedSeq[BaseWrap] = IndexedSeq()

   def nameSubterms(t:Int):Map[Name,BaseWrap] = Map()

   def isAtom(t:Int) = false

   def is[X](t:Int)(implicit ttag:TypeTag[Int], xtag:TypeTag[X]):Boolean =
                               (typeOf[X] <:< typeOf[Int]) 
     
   def value[X](t:Int)(implicit mt:Manifest[Int], mx:Manifest[X]):Option[X] =
      if (mx <:< implicitly[Manifest[Int]]) {
         Some(t.asInstanceOf[X])
      } else {
         None
      }


}


// vim: set ts=4 sw=4 et:
