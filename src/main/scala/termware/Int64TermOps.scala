package termware

trait Int64TermOps extends NumericTermOps {

 this: Int64Term =>

 override def withAttributes(newAttributes: Map[Name,Term]) = copy(attributes = newAttributes)

}
