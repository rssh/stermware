package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;


/**
 * strategy for applying of matchibg net to term.
 **/
trait Strategy
{

   def apply(t:Term, mn: MatchingNet)(implicit ctx:CallContext):ComputationBounds[Pair[ComputationBounds[Term],Boolean]]
   
}

class FirstLeft extends Strategy
{

 def apply(t:Term,matchingNet:MatchingNet)(implicit ctx:CallContext):ComputationBounds[Pair[ComputationBounds[Term],Boolean]] =
 {
   matchingNet.doMatch(t)  match {
      case Right(matchingNet.Success(s,node,optBranch)) => 
                                    if (optBranch!=None) {
                                        Done((optBranch.get.subst(s),true));
                                     } else {
                                        // impossible.
                                        throw new TermWareException("branch without result");
                                     }
      case Left(matchingNet.Failure(t,reason)) => CallCC.compose(
                                     applySubterms(matchingNet,t.subterms,0),
                                     { (x:Pair[IndexedSeq[ComputationBounds[Term]],Boolean],ctx:CallContext) =>
                                       implicit val ictx = ctx;
                                       if (x._2) {
                                         Done((t.signature.createTerm(t.name,x._1),true))
                                       } else {
                                         Done((Done(t),false));
                                       }
                                     });
  }
 }

 def applySubterms(matchingNet:MatchingNet, subterms: IndexedSeq[Term], i:Int)(implicit ctx:CallContext):
          ComputationBounds[Pair[IndexedSeq[ComputationBounds[Term]],Boolean]] =
 {
   if (i >= subterms.length) {
       Done(subterms map (Done(_)),false);
   } else {
      CallCC.compose(apply(subterms(i),matchingNet),{
                        (x:Pair[ComputationBounds[Term],Boolean],ctx:CallContext) =>
                        implicit val ictx = ctx;
                        if (x._2) {
                            Done(((0 to subterms.length) map ( (j:Int) => if (i==j) x._1 else Done(subterms(j))),true))
                        } else {
                            applySubterms(matchingNet,subterms,i+1);
                        }
                     });
  }
 }
  

}
