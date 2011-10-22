package ua.gradsoft.termware.flow;

import scala.util.control.ControlThrowable;

class CallCCThrowable[A](val current:ComputationBounds[A])
                        (implicit val ctx:CallContext) extends ControlThrowable;


