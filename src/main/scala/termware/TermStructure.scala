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

}

case class DefaultTermStructure(name:Name, componentNames: IndexedSeq[Name]) extends TermStructure
{

  val componentIndexes = componentNames.foldLeft(Map[Name,Int]()){ (s,e) => s.updated(e,s.size+1) }

  override def componentIndex(name:Name): Option[Int] = componentIndexes.get(name)

  override def componentName(i:Int): Option[Name] = 
     if (i < componentNames.size) {
         Some(componentNames(i))
     } else None

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
  
}

object SeqTermStructure
{
  val typeIndex = 2
}

object TermStructure
{

   def write(ts: TermStructure, out:Output): Unit =
    ts match {
      case DefaultTermStructure(name,components) =>
             out.writeInt(DefaultTermStructure.typeIndex)
             free.Serializer.writeName(ts.name,out)
             val nComponents = components.size
             out.writeInt(nComponents)
             for(i <- 1 to nComponents) {
                val component = components(i)
                free.Serializer.writeName(component,out)
             }
       case SeqTermStructure(name:Name) =>
             out.writeInt(SeqTermStructure.typeIndex)
             free.Serializer.writeName(ts.name,out)
    }

   def read(in: Input): TermStructure = 
   {
     val typeIndex = in.readInt
     typeIndex match {
       case DefaultTermStructure.typeIndex =>
             val name = free.Serializer.readName(in)
             val arity = in.readInt
             val s0 = IndexedSeq[Name]()
             val componentNames = (1 to arity).foldLeft(s0){ (s,i)=>
                                    s :+ free.Serializer.readName(in)
                                  }
             DefaultTermStructure(name,componentNames)
       case SeqTermStructure.typeIndex =>
             val name = free.Serializer.readName(in)
             SeqTermStructure(name)
       case _ =>
             throw new IllegalStateException("Unknown term-structure type index: typeIndex");
     }
   }

}
