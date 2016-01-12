package termware


trait TermEmptyComponents
{

  this: Term =>

  override def arity = 0

  override def component(n:Name) = None

  override def component(i:Int) = None

  override def componentName(i:Int) = None

  override def componentIndex(n:Name) = None

  override def isScope = true

  override def scopeArity = 0

  override def scopeVar(n: Name) = None

  override def scopeVar(i: Int) = None

  override def scopeVarIndex(n: Name) = None

  override def scopeVarName(i: Int) = None

}

