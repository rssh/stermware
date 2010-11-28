package ua.gradsoft.termware.flow;

class CallCCException[A](val current:ComputationBounds[A])
                        (implicit val ctx:CallContext) extends Exception;


