package termware


trait PrimitiveTermOps extends TermEmptyComponents
{
  this: PrimitiveTerm =>

  override def isVar = false

  override def varIndex = -1

  override def isScoped = false

  override def scopeIndex = -1

  override def resolve(t: Term) = None

}
