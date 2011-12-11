package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

object CbTermUtil
{

  def cb(s:PartialFunction[Term,Term])(implicit ctx:CallContext)
                                         :PartialFunction[CbTerm,CbTerm] =
   new PartialFunction[CbTerm,CbTerm] () {

            def isDefinedAt(x:CbTerm)
                   = s.isDefinedAt(x.toTerm);

            def apply(x:CbTerm) =
                   s.apply(x.toTerm).toCbTerm

   };

  def cb(s:Substitution[Term])(implicit ctx:CallContext) : Substitution[CbTerm] =
    new Substitution[CbTerm]() {

            def isDefinedAt(x:CbTerm)
                   = s.isDefinedAt(x.toTerm);

            def apply(x:CbTerm) =
                   s.apply(x.toTerm).toCbTerm

            def  lastZipIndex = s.lastZipIndex;

            def  withIndex(zi:BigInt) = cb(s.withIndex(zi));

            def +(kv:(CbTerm,CbTerm))(implicit ctx:CallContext)
                               : ComputationBounds[(Boolean,Substitution[CbTerm])] =
               CallCC.compose(
                      s+(kv._1.toTerm,kv._2.toTerm),
                      { x:(Boolean,Substitution[Term]) => Done((x._1, cb(x._2))) }
               );

    };
    
  

  def uncb(s:PartialFunction[CbTerm,CbTerm])(implicit ctx:CallContext):PartialFunction[Term,Term] =
   new PartialFunction[Term,Term] () {

            def isDefinedAt(x:Term)
                   = s.isDefinedAt(x.toCbTerm);

            def apply(x:Term) =
                   s.apply(x.toCbTerm).toTerm

   };


  def uncb(s:Substitution[CbTerm])(implicit ctx:CallContext):Substitution[Term] =
    new Substitution[Term]() {

            def isDefinedAt(x:Term)
                   = s.isDefinedAt(x.toCbTerm);

            def apply(x:Term) =
                   s.apply(x.toCbTerm).toTerm

            def lastZipIndex = s.lastZipIndex;

            def withIndex(zi:BigInt) = uncb(s.withIndex(zi));

            def +(kv:(Term,Term))(implicit ctx:CallContext)
                               : ComputationBounds[(Boolean,Substitution[Term])] =
               CallCC.compose(
                      s+(kv._1.toCbTerm,kv._2.toCbTerm),
                      { x:(Boolean,Substitution[CbTerm]) => Done((x._1, uncb(x._2))) }
               );

    };

}
