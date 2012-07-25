package termware;

import scala.util.control.ControlThrowable;
import scala.reflect.runtime.universe._

class CallCCThrowable[A](val current:ComputationBounds[A])
                        (implicit val aTag: TypeTag[A])
                                             extends ControlThrowable
{
  type AType = A;
  def aType = typeOf[A]
}


