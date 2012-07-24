package ua.gradsoft.termware.flow;

import scala.util.control.ControlThrowable;
import scala.reflect.Manifest;

class CallCCThrowable[A](val current:ComputationBounds[A])
                         (implicit val ctx:CallContext,
                          implicit val m: Manifest[A])
                                             extends ControlThrowable
{
  type aType = A;
  def aManifest: Manifest[A] = m;
}


