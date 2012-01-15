package ua.gradsoft.termware;

import scala.collection.mutable.{HashMap => MutableHashMap};
import java.io.PrintWriter;
import ua.gradsoft.termware.flow._;

abstract class FunctionalTerm(s:TermSignature) extends Term
                                             with ComplexUnify
                                             with ComplexSubst
                                             with ComplexCompare
                                             with NonNumberTerm
                                             with NonBooleanTerm
{

  def isNil: Boolean = false;
  def isAtom: Boolean = false;
  def isEta: Boolean = false;
  def isError: Boolean = false;
  
  def optValue[T](implicit mt:Manifest[T]) = s.to[T](this);

  def unify(t:Term, s: Substitution[Term])(implicit ctx:CallContext):
                           ComputationBounds[(Boolean,Substitution[Term])] =
     ctx.withCall{
      (ctx:CallContext) => implicit val ictx = ctx;
        if (patternName==t.patternName && arity==t.arity) {
           unifySeq(subterms,t.subterms,s);      
        } else {
           Done(false,s);
        }
     }

  def unifySeq(x:Seq[Term],y:Seq[Term],s:Substitution[Term])
              (implicit ctx: CallContext): 
                              ComputationBounds[(Boolean,Substitution[Term])] =
  {
    if (x.isEmpty) {
       Done((y.isEmpty,s))
    } else if (y.isEmpty) {
       Done((false,s))
    } else {
      ctx.withCall{
         (ctx:CallContext) => implicit val ictx = ctx;
            x.head.onUnify(y.head,s){
             (rs: (Boolean,Substitution[Term]),ctx:CallContext) => 
                 implicit val ictx = ctx;
                 if (rs._1) {
                    unifySeq(x.tail,y.tail,rs._2);
                 } else {
                    Done(false,rs._2);
                 }
         }
      }
    }
  }

  def subst(s: PartialFunction[Term,Term])(implicit ctx:CallContext)
                                               :ComputationBounds[Term]  = 
  ctx.withCall{
     (ctx:CallContext) => implicit val ictx = ctx;
     if (s.isDefinedAt(this)) {
        Done(s(this));
     } else {
       val nSubterms = for(st <-subterms) yield {
                         if (s.isDefinedAt(st)) {
                            Done(s(st)); 
                         } else {
                            st.subst(s);
                         }
                     }
       val seqCb = CallCC.seq(nSubterms);
       val retval = CallCC.compose(seqCb,
             { x:IndexedSeq[Term] => Done(signature.createTerm(name,x)) });
       retval;
     }
  };

  def termClassIndex = TermClassIndex.FUNCTIONAL;

  def termCompare(t:Term)(implicit ctx:CallContext):ComputationBounds[Int] = 
  {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) {
      Done(c)
    } else { 
       c = arity - t.arity;
       if (c!=0) 
          Done(c)
       else {
          c = name.compareTo(t.name);
          if (c!=0) Done(c) else termCompareSeq(subterms,t.subterms)
       }
    }
  }

  def termCompareSeq(x:Seq[Term],y:Seq[Term])(implicit ctx:CallContext)
       :ComputationBounds[Int] = 
  ctx.withCall {
     (ctx:CallContext) => implicit val ictx=ctx;
     if (x.isEmpty) 
         Done(if (y.isEmpty) 0 else -1)
     else
       x.head.onTermCompare(y.head){
         (c:Int, ctx:CallContext) => if (c!=0) 
                                       Done(c)
                                     else 
                                       termCompareSeq(x.tail,y.tail)(ctx);
       }
  } 

  override def print(out:PrintWriter):Unit = {
     out.print(name.string);
     out.print("(");
     var frs = true;
     for(t<-subterms) {
        if (frs) {
          frs=false;
        }else{
          out.print(", ");
        }
        t.print(out);
     }
     out.print(")");
  }


  override def toString = name.toString+"("+subterms.mkString(",")+")";
  
  lazy val attributes = new MutableHashMap[Name,Term]();

  val signature=s;

}


object FunctionalTerm
{

   def unapply(t:Term):Option[Tuple3[Name,IndexedSeq[Term],TermSignature]] =
     if (t.arity > 0) {
       Some((t.name,t.subterms,t.signature));
     } else {
       None;
     }

}

object StrictFunctionalTerm
{
   def unapply(t:FunctionalTerm):Option[Tuple3[Name,IndexedSeq[Term],TermSignature]] =
     Some((t.name,t.subterms,t.signature));
}


