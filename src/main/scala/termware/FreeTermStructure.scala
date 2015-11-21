package termware

case class FreeTermStructure(override val name:Name) extends TermStructure
{

   override def componentIndex(name: Name): Option[Int] = 
     name match {
        case IntName(i) => Some(i)
        case _          => None
     }
      
   override def componentName(i: Int): Option[Name] = 
      Some(IntName(i))

}

