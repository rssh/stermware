package termware

trait DoubleTermOps extends NumericTermOps {

 this: DoubleTerm =>

  override def withAttributes(newAttributes: Map[Name,Term]) = copy(attributes = newAttributes)

}
