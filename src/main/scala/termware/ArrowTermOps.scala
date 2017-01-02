package termware

trait ArrowTermOps extends TermOps {

 this: ArrowTerm =>

 override def name = ArrowName

 override def cardinality = TermCardinality.ONE

 

}
