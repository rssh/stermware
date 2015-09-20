package termware

trait TermStructure
{
  def name: Name

  def componentIndexes: Map[Name,Int]

  def componentNames: IndexedSeq[Name]
}

case class DefaultTermStructure(name:Name, componentNames: IndexedSeq[Name]) extends TermStructure
{

  val componentIndexes = componentNames.foldLeft(Map[Name,Int]()){ (s,e) => s.updated(e,s.size+1) }

}
