package termware


trait TermEmptyComponents
{

  this: Term =>

  def arity = 0

  def component(n:Name) = None

  def component(i:Int) = None

  def componentNames:IndexedSeq[Name] = IndexedSeq()

}

