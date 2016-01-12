package termware

trait ScopeTermOps extends TermOps
{
  this: ScopeTerm =>

  def   arity = body.arity

  def   component(n: Name) = body.component(n) map (this.substitute(_))
  def   component(i: Int) = body.component(i) map (this.substitute(_))

  def   componentIndex(n: Name): Option[Int] = body.componentIndex(n)
  def   componentName(i: Int): Option[Name] = body.componentName(i)

  def isScope: Boolean = true

  def isScoped: Boolean = body.isScoped
  def isVar: Boolean = 
       if (body.isVar && body.scopeIndex==scopeIndex)
           vars(body.varIndex)._2.isVar 
       else
           false

  def name = body.name
  def scopeArity: Int = vars.size
  def scopeVar(n: Name): Option[Term] =
      varNames.get(n).flatMap(scopeVar(_))

  def scopeVar(i: Int): Option[Term] = varsi(i) map (_._2)

  def scopeVarIndex(n: Name): Option[Int] = ???
  def scopeVarName(i: Int): Option[Name] = ???
  def varIndex: Int = -1

  // TODO: generalizw to any term.
  def resolve(t:Term): Option[Term] =
  {
    def resolveSucc(b:Term,t: Term): Term =
    {
      b.resolve(t).getOrElse(t)
    }
    if (t.arity == 0) {
        scopeVar(t.name) match {
          case Some(t1) => Some(resolveSucc(body,t1))
          case None => body.resolve(t)
        }
    }else{
        //TODO: implement all
        None
    }
  }

  def withAttributes(attributes: Map[Name,Term]): Term = 
        copy(attributes=attributes)

  private def varsi(i: Int): Option[(VarTerm,Term)] =
     if (i<vars.length) {
       None
     }else{
       Some(vars(i))
     }

  private def substitute(t: Term): Term =
     t match {
       case x: AtomTerm => t
       case x: PrimitiveTerm => t
       case x: StructuredTerm => ScopeTerm(scopeIndex,vars,t)
       case x: ScopeTerm => ScopeTerm(scopeIndex,vars,t)  // TODO: think about implementing mergeScope ?
       case x: VarTerm => if (x.scopeIndex == scopeIndex) {
                             val (m,st) = vars(x.varIndex)
                             if (st eq x) {
                                 x
                             }else{
                                 substitute(st)
                             }
                          } else x
     }


  lazy val varNames:Map[Name,Int] = ???
}
