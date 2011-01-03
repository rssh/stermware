package ua.gradsoft.termware;

import scala.collection.Set;
import scala.collection.immutable.Map;
import scala.collection.immutable.TreeMap;
import scala.collection.mutable.HashMap;
import java.io.PrintWriter;
import ua.gradsoft.termware.fn._;
import ua.gradsoft.termware.util._;
import ua.gradsoft.termware.flow._;


class EtaTerm(v:Set[EtaXTerm], l:Term, r:Term, rs:Term, s:EtaTermSignature)  
                             extends EtaXOwner
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

  override def arity = 3;

  override def subterms = IndexedSeq(left, right, rest);

  override def subterm(index: Int) = 
    index match {
      case 0 => left
      case 1 => right
      case 2 => rest
      case _ => throwUOE
    }

  override def unify(t:Term,s:Substitution)(implicit ctx:CallContext)
                               :ComputationBounds[(Boolean,Substitution)]=
  {
    if (t.isEta) {
      ctx.withCall{
       (ctx:CallContext) => implicit val ictx = ctx;
       val tLeft = t.subterms(0);
       left.onUnify(tLeft,s){
         (rs:(Boolean,Substitution), ctx: CallContext) =>
           implicit val ictx = ctx;
           val (r,s) = (rs._1, rs._2);
           if(r) {
             val tRight = t.subterms(1);
             right.onUnify(tRight,s){
               (rs:(Boolean,Substitution),ctx:CallContext) =>
                 implicit val ictx = ctx;
                 val r = rs._1;
                 val s = rs._2;
                 if(r) {
                   val tRest = t.subterms(2);
                   rest.unify(tRest,s);
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
       val s1 = new PartialFunction[Term,Term]{

                  def isDefinedAt(t:Term):Boolean =
                     t match {
                        case x:EtaXTerm =>
                           (x.xOwner==EtaTerm.this)
                        case _ =>
                             false;
                     }

                  def apply(t:Term):Term =
                     t match {
                        case x:EtaXTerm =>
                           if (x.xOwner==EtaTerm.this) 
                             new EtaXTerm(x.name,x.xLabel,
                                          x.typeTerm,
                                          null,
                                          signature.theory.etaXSignature)
                           else
                             x
                        case _ => t
                     }
                  
                }.orElse(s);
       val substituted = CallCC.tuple(
               Call{ (ctx:CallContext)=>left.subst(s1)(ctx) },
               Call{ (ctx:CallContext)=>right.subst(s1)(ctx) },
               Call{ (ctx:CallContext)=>rest.subst(s1)(ctx) }
       );
       CallCC.compose(substituted,
              { tuple:Tuple3[Term,Term,Term] =>
                    Done(
                      new EtaTerm(vars,tuple._1,tuple._2,tuple._3,signature)) 
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
                    rs.termCompare(t.subterm(2))
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
    if (rest.name!=ID) {
      out.print("|");
      rest.print(out);
    }
  }

  var attributes=new HashMap[Name,ComputationBounds[Term]]();

  //private def newVars(vn:Set[AtomTerm]):Map[Term,Term]=
  //  {
  //    var r:Map[Term, Term] = TreeMap.empty;
  //    for(v:Term <- vars) {
  //        r=r+(v -> new EtaXTerm(v.name, v.xLabel,null));
  //    }
  //    r;
  //  }


  private val vars: Set[EtaXTerm] = v;
  private val left: Term = l;
  private val right: Term = r;
  private val rest: Term = rs;
          val signature = s;
  for(x <- v) x.setOwner(this);
       
  private lazy val hash: Int = left.hashCode+right.hashCode+rest.hashCode;

  lazy val ID = signature.theory.symbolTable.getOrCreate("id");

}
