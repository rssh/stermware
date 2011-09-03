package ua.gradsoft.termware;

import scala.collection.Set;
import scala.collection.immutable.Map;
import scala.collection.immutable.TreeMap;
import scala.collection.mutable.HashMap;
import java.io.PrintWriter;
import ua.gradsoft.termware.fn._;
import ua.gradsoft.termware.util._;
import ua.gradsoft.termware.flow._;


class EtaTerm(vars:IndexedSeq[XTerm], l:Term, r:Term, rs:Option[Term], s:EtaTermSignature)  
                             extends XOwner(vars)
                                  with ComplexUnify
                                  with ComplexSubst
                                  with ComplexCompare
                                  with NonNumberTerm
{

  def isNil = false;

  def isAtom = false;

  def isEta = true;

  def isError = false;

  override def name = signature.fixedName.get;

  override def arity = if (rest==None) 2 else 3;

  override def subterms = if (rest==None) IndexedSeq(left, right) else IndexedSeq(left, right, rest.get);

  override def subterm(index: Int) = 
    index match {
      case 0 => left
      case 1 => right
      case 2 => if (rest!=None) rest.get else throwUOE;
      case _ => throwUOE
    }

  override def unify(t:Term,s:Substitution[Term])(implicit ctx:CallContext)
                               :ComputationBounds[(Boolean,Substitution[Term])]=
  {
    if (t.isEta) {
      ctx.withCall{
       (ctx:CallContext) => implicit val ictx = ctx;
       val tLeft = t.subterms(0);
       left.onUnify(tLeft,s){
         (rs:(Boolean,Substitution[Term]), ctx: CallContext) =>
           implicit val ictx = ctx;
           val (r,s) = (rs._1, rs._2);
           if(r) {
             val tRight = t.subterms(1);
             right.onUnify(tRight,s){
               (rs:(Boolean,Substitution[Term]),ctx:CallContext) =>
                 implicit val ictx = ctx;
                 val r = rs._1;
                 val s = rs._2;
                 if(r) {
                   rest match {
                      case None => Done(((t.arity==2),s))
                      case Some(xRest) =>
                         val tRest = t.subterms(2);
                         xRest.unify(tRest,s);
                   }
                 } else {
                   Done((false,s));
                 }
             }
           } else {
             Done((false,s));
           } 
       }
      }
    }else{
       Done((false,s));
    }
  }

  override def subst(s:PartialFunction[Term,Term])
                    (implicit ctx: CallContext) :
                                                  ComputationBounds[Term] = 
    ctx.withCall { 
       (ctx:CallContext) => implicit val ictx = ctx;
       val (newVars, s1) = copyVarFun(s);
       val substituted = CallCC.tuple(
               Call{ (ctx:CallContext)=>left.subst(s1)(ctx) },
               Call{ (ctx:CallContext)=>right.subst(s1)(ctx) },
               Call{ (ctx:CallContext)=> rest match {
                                           case None => Done(None)
                                           case Some(xRest) => CallCC.some(xRest.subst(s1))(ctx) 
                                         } 
                   }
       );
       CallCC.compose(substituted,
              { tuple:Tuple3[Term,Term,Option[Term]] =>
                    Done(
                      new EtaTerm(newVars,tuple._1,tuple._2,tuple._3,signature)) 
              });
    }

  override def termClassIndex  = TermClassIndex.ETA;

  override def termHashCode = hashCode;

  override def termCompare(t:Term)(implicit ctx:CallContext)
                                                :ComputationBounds[Int] = {
     val c = termClassIndex - t.termClassIndex;
     if (c!=0) {
        Done(c);
     } else {
        left.onTermCompare(t.subterm(0)){
           (c:Int,ctx:CallContext) => implicit val ictx=ctx;
             if (c!=0) 
               Done(c)
             else
               right.onTermCompare(t){
                 (c:Int,ctx:CallContext) => implicit val ictx=ctx;
                  if (c!=0) 
                    Done(c)
                  else
                    rs match {
                      case None => Done(if (t.arity==2) 0 else -1)
                      case Some(x) => x.termCompare(t.subterm(2))
                    }
               }
        }
     }
  }

  override def print(out:PrintWriter):Unit = {
    out.print("var (");
    var frs = true;
    for(x <- vars) {
       if (!frs) {
          out.print(", ");
       }else{
          frs=false;
       }
       x.print(out);
    }
    out.print("):");
    left.print(out);
    out.print("->");
    right.print(out);
    if (rest!=None) {
      out.print("|");
      rest.get.print(out);
    }
  }

  var attributes=new HashMap[Name,ComputationBounds[Term]]();


  private val left: Term = l;
  private val right: Term = r;
  private val rest: Option[Term] = rs;
          val signature = s;
       
  private lazy val hash: Int = left.hashCode+right.hashCode+rest.hashCode;

}
