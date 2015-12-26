package termware

trait CharTermOps {

 this: CharTerm =>

 override def withAttributes(newAttributes: Map[Name,Term]) =
     copy(attributes = newAttributes)

}
