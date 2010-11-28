package ua.gradsoft.termware.flow;

class CallCCException[A](val current:ComputationBounds[A],
                         val ctx:CallContext) extends Exception;


