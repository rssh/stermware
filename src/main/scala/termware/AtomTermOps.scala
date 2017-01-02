package termware


trait AtomTermOps extends UniTermOps
{
  this: AtomTerm =>

  override val name = AtomName(value)

  override def arity: Int = 0

  override def uniTerm() = this
}
