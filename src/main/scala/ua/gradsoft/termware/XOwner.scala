package ua.gradsoft.termware;


/**
 * term, which have bound variables.
 **/
abstract class XOwner(val vars:IndexedSeq[XTerm]) extends Term
{
  for(x <- vars) x.xOwner = this;


  def copyVarFun: (IndexedSeq[XTerm], PartialFunction[Term,Term]) =
  {
    val newVars = vars.map(x=>new XTerm(x.name,x.xLabel,x.typeTerm,null,
                                      signature.theory.xSignature));
    val s1 = new PartialFunction[Term,Term]{

                  def isDefinedAt(t:Term):Boolean =
                     t match {
                        case x:XTerm =>
                           (x.xOwner eq XOwner.this)
                        case _ =>
                             false;
                     }

                  def apply(t:Term):Term =
                     newVars(t.asInstanceOf[XTerm].xLabel);

                };

    return (newVars,s1);
  }

}
