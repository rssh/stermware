package termware

trait TermVM
{

   def put(t: Term)

   def step(): Boolean

   def finalState: Boolean

   def errorState: Boolean

   def result(): TrackedResult[Term]

}
