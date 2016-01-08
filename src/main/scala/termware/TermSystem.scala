package termware


trait TermSystem
{
   def name: Term

   def adopt(t: Term): TermWithContext = ???

   def unification: Unification = free.Unification

   def resolve(name: Term): TrackedResult[Term] = ??? 

   def storage: TermStorage = ??? // baseTermStorage(name)

   def serializer: TermSerializer = free.Serializer

   def isFree: Boolean = false

   def createVM(): TermVM
}


