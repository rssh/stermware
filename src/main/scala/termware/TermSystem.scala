package termware

import scala.reflect.runtime.universe._



trait TermSystem
{

   def toTerm[T](t:T)(implicit ttag: TypeTag[T]): Term
  

}
