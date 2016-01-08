package termware

sealed trait TrackedResult[+A]
{
   def result(): A
   def isSuccess: Boolean
   def isFailure: Boolean
   def tracking() : Seq[String]

   def map[B](f: A=>B): TrackedResult[B]
   def flatMap[B](f:A=>TrackedResult[B]): TrackedResult[B]

}


case class TrackedSuccess[+A](override val result: A, override val tracking:Seq[String]) extends TrackedResult[A]
{
   override def isSuccess = true
   override def isFailure = false

   override def map[B](f: A=>B) = TrackedSuccess(f(result),tracking)
   override def flatMap[B](f: A=>TrackedResult[B]): TrackedResult[B] = 
                 f(result) match {
                   case TrackedSuccess(bResult, bTracking) =>
                          TrackedSuccess(bResult, tracking ++ bTracking)
                   case TrackedFailure(message, bTracking) =>
                          TrackedFailure(message, tracking ++ bTracking)
                 }
}

case class TrackedFailure(message:String, override val tracking:Seq[String]) extends TrackedResult[Nothing]
{
   override def result(): Nothing =
    throwPE("failure",tracking)

   override def isSuccess = false
   override def isFailure = true

   override def map[B](f: Nothing => B) = this
   override def flatMap[B](f: Nothing => TrackedResult[B]) = this

}

object TrackedFailure
{
   def apply(message:String): TrackedFailure = apply(message, Seq())
}

