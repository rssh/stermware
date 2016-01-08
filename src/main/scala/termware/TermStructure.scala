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

 // def component(i:Int, t:Term): Option[Term]
//:w
 // def component(n:Name, t:Term): Option[Term] =
 //      componentIndex(n).flatMap(component(_,t))

  /**
   * is this term define a scope ?
   */
  def isScope: Boolean

  def varIndex(name:Name): Option[Int]

}

/**
 * default term structure
 * TODO: add vars
 **/
case class DefaultTermStructure(name:Name, componentNames: IndexedSeq[Name]) extends TermStructure
{

  val componentIndexes = componentNames.foldLeft(Map[Name,Int]()){ (s,e) => s.updated(e,s.size+1) }

  override def componentIndex(name:Name): Option[Int] = componentIndexes.get(name)

  override def componentName(i:Int): Option[Name] = 
     if (i < componentNames.size) {
         Some(componentNames(i))
     } else None

  def isScope: Boolean = false

  def varIndex(name:Name): Option[Int] = None

}

object DefaultTermStructure
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
  
  def isScope: Boolean = false

  def varIndex(name:Name): Option[Int] = None

}

object SeqTermStructure
{
  val typeIndex = 2
}

object TermStructure
{

   def write(ts: TermStructure, out:Output): Unit =
   {
    ts match {
      case DefaultTermStructure(name,components) =>
             out.writeInt(DefaultTermStructure.typeIndex)
             //out.writeBoolean(isScope)
             free.Serializer.writeName(ts.name,out)
             val nComponents = components.size
             out.writeInt(nComponents)
             for(i <- 1 to nComponents) {
                val component = components(i)
                free.Serializer.writeName(component,out)
             }
       case SeqTermStructure(name) =>
             out.writeInt(SeqTermStructure.typeIndex)
             //out.writeBoolean(isScope)
             free.Serializer.writeName(ts.name,out)
    }
  }

   def read(in: Input): TermStructure = 
   {
     val typeIndex = in.readInt
     //val isScope = (in.readByte != 0)
     val name = free.Serializer.readName(in)
     typeIndex match {
       case DefaultTermStructure.typeIndex =>
             val arity = in.readInt
             val s0 = IndexedSeq[Name]()
             val componentNames = (1 to arity).foldLeft(s0){ (s,i)=>
                                    s :+ free.Serializer.readName(in)
                                  }
             DefaultTermStructure(name,componentNames)
       case SeqTermStructure.typeIndex =>
             SeqTermStructure(name)
       case _ =>
             throw new IllegalStateException("Unknown term-structure type index: typeIndex");
     }
   }

}
