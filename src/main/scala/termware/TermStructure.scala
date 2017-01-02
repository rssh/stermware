package termware

import util._

/**
 * Simple description: what are name of term and names of components.
 **/
sealed trait TermStructure
{

  def name: Name

  def componentIndex(name:Name): Option[Int]

  def componentName(i: Int): Option[Name]

  def component(i:Int, t: StructuredTerm): Option[Term] = t.components.lift(i)

  def component(n:Name, t:StructuredTerm): Option[Term] =
       componentIndex(n).flatMap(component(_,t))

}

/**
 * simple term structure which consists just from sequence of names
 **/
case class SimpleTermStructure(name:Name, componentNames: IndexedSeq[Name]) extends TermStructure
{

  val componentIndexes = componentNames.foldLeft(Map[Name,Int]()){ (s,e) => s.updated(e,s.size+1) }

  override def componentIndex(name:Name): Option[Int] = componentIndexes.get(name)

  override def componentName(i:Int): Option[Name] = 
     if (i < componentNames.size) {
         Some(componentNames(i))
     } else None

  def varIndex(name:Name): Option[Int] = None

}

object SimpleTermStructure
{
  val typeIndex = 1
}

case class SeqTermStructure(name:Name) extends TermStructure
{

  override def componentIndex(name:Name): Option[Int] =
   name match {
     case IntName(v) => Some(v)
     case _          => None
   }

  override def componentName(i:Int): Option[Name] = Some(IntName(i))
  
  def varIndex(name:Name): Option[Int] = None

}

object SeqTermStructure
{
  val typeIndex = 2
}

object TermStructure
{

}
