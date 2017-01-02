package termware

trait Int32TermOps extends NumericTermOps
{
 this: Int32Term =>

 override val name = IntName(value)

}
