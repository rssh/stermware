package termware

import scala.reflect.runtime.universe._



trait TermSystem
{

   def toTerm[T](t:T)(implicit ttag: TypeTag[T]): Either[String,Term]

   def fromTerm[T](t: Term)(implicit ttag: TypeTag[T]): Either[String,T]

}
