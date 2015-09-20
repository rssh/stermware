package termware


trait TermAttributes 
{
  this: AttributedTerm =>

  val attributes: Map[Name,Term] = Map()


}
