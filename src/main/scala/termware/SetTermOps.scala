package termware

trait SetTermOps extends TermOps {

 this: SetTerm =>

 override def name = SetName

 override def cardinality = TermCardinality.FINITE

}
