package termware


trait TermSystem
{
   def name: Term

   def adopt(t: Term): TermWithContext = ???

   def unification: Unification = free.Unification

   def resolve(name: Term): Option[Term] = ??? // Todo: s/Option/ResolveResult

   def storage: TermStorage = ??? // baseTermStorage(name)

   def serializer: TermSerializer = free.Serializer

   def isFree: Boolean = false
}


