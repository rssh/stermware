package termware

trait StringTermOps extends PrimitiveTermOps
{
 this: StringTerm => 

 override val name = StringName(value)
}
