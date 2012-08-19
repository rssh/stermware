package termware;

import scala.util.control.ControlThrowable;

class CallCCThrowable[A](val trampolineId:TrampolineId,
                         val current:ComputationBounds[A])
                                             extends ControlThrowable
{
   def tid = trampolineId
}


