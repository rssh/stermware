package ua.gradsoft.termware;


/**
 * term, which have bound variables.
 **/
abstract class XOwner(val vars:IndexedSeq[XTerm]) extends Term
{
  for(x <- vars) x.xOwner = this;

  /**
   * usually XOwner have no value representations.
   */
  def optValue[T](implicit mt:Manifest[T]):Option[T] = None;

  def copyVarFun(s:PartialFunction[Term,Term]): 
                    (IndexedSeq[XTerm], PartialFunction[Term,Term]) =
  {
    val newVars = vars.flatMap{
         (x:XTerm)=> if (s.isDefinedAt(x)) {
                        None
                     } else {
                         Some(new XTerm(x.name,x.xLabel,x.typeTerm,null,
                                      signature.theory.xSignature))
                     }
    };
    val s1 = s.orElse(new PartialFunction[Term,Term]{

                  def isDefinedAt(t:Term):Boolean =
                     t match {
                        case x:XTerm =>
                           (x.xOwner eq XOwner.this)
                        case _ =>
                             false;
                     }

                  def apply(t:Term):Term =
                     newVars(t.asInstanceOf[XTerm].xLabel);

                });
    return (newVars,s1);
  }

}
