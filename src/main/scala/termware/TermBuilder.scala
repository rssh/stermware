package termware

// TODO: make one mutable.
case class TermBuilder(
  private val parent:     Option[TermBuilder],
  private val structure:  TermStructure,
  private val components: IndexedSeq[Term]=IndexedSeq(),
  private val scopeIndex: Int,
  private val lastScopeIndex: Int,
  private val nVariables: Int,
  private val termSystem: TermSystem
                 )
{


  def startChild(ts: TermStructure) : TermBuilder =
  {
   val (newScopeIndex, newLastScopeIndex) = if (ts.isScope) {
                                        val newScopeIndex = lastScopeIndex+1 
                                        (newScopeIndex, newScopeIndex)
                                      } else (-1, lastScopeIndex)
   new TermBuilder(Some(this),ts,IndexedSeq(), newScopeIndex, newLastScopeIndex, 0, termSystem)
  }

  def finishChild(): TermBuilder =
  {
   parent.get.addTermWithScope(StructuredTerm(structure,components,Map(),termSystem),lastScopeIndex)
  }

  def addTermWithScope(t: Term, newLastScopeIndex: Int): TermBuilder =
   copy(components = components :+ t, lastScopeIndex = lastScopeIndex)

  def addPrimitive(t: PrimitiveTerm): TermBuilder =
   copy(components = components :+ t)

  def addAtom(t: AtomTerm): TermBuilder =
   copy(components = components :+ t)


}
