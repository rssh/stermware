package termware

trait TermStructure
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
