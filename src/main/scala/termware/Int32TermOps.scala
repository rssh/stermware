package termware

trait Int32TermOps extends NumericTermOps {

 this: Int32Term =>

 override def withAttributes(newAttributes: Map[Name,Term]) = copy(attributes = newAttributes)

}
