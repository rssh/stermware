package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import ua.gradsoft.termware.flow._;

class ListTerm(h:Term, t:Term, s:ListTermSignature) 
                                        extends FunctionalTerm(s)
{

  def arity: Int = 2;

  def subterm(i:Int):Term = 
    i match {
      case 0 => head
      case 1 => tail
      case _ => throw new IndexOutOfBoundsException();
    }

  def subterms:IndexedSeq[Term] = IndexedSeq[Term](head,tail);

  override def unify(t:Term, s: Substitution[Term])
         (implicit ctx:CallContext):ComputationBounds[(Boolean,Substitution[Term])]=
  {
   if (t.arity==2)
    ctx.withCall{
      (ctx:CallContext)=>implicit val ictx=ctx;
      head.onUnify(t.subterm(0),s) {
         (r:(Boolean,Substitution[Term]),ctx:CallContext) => implicit val ictx=ctx;
         if (r._1) {
           tail.unify(t.subterm(1),r._2);
         }else{
           Done((false,r._2))
         }
      }
    }
   else
    Done((false,s))
  }

  override def subst(s: PartialFunction[Term,Term])
                           (implicit ctx:CallContext):ComputationBounds[Term] =
      signature.createTerm(name,head.subst(s),tail.subst(s));

  override def termCompare(t:Term)(implicit ctx:CallContext):
                                          ComputationBounds[Int] = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) 
       Done(c)
    else {
       c = arity - t.arity;
       if (c!=0) 
         Done(c)
       else {
         c = name.compareTo(t.name);
         if (c!=0) {
           Done(c);
         } else {
           head.onTermCompare(t.subterm(0)){
             (c:Int, ctx:CallContext) => implicit val ictx = ctx;
             if (c!=0) 
               Done(c)
             else
               tail.termCompare(t.subterm(1)) 
           }
        }
      }
   }
  }

  def name = signature.fixedName.get;

  override def toString = "[" + toStringI(this);

  def toStringI(t:Term):String = t match {
    case l:ListTerm => l.subterm(0).toString +
                              (if (l.subterm(1).isNil) {
                                 "]"
                               } else {
                                 "," +toStringI(l.subterm(1))
                               });
    case _ => t.toString
  }

  lazy val termHashCode = name.hashCode+head.hashCode+tail.hashCode;
  
  val head = h;
  val tail = t;

}
