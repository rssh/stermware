package ua.gradsoft.termware;

import scala.collection.Set;
import scala.collection.immutable.Map;
import scala.collection.immutable.TreeMap;
import scala.collection.mutable.{HashMap => MutableHashMap};
import java.io.PrintWriter;
import ua.gradsoft.termware.util._;
import ua.gradsoft.termware.flow._;


class WithTerm(override val vars:IndexedSeq[XTerm], val p:Term, val ws: WithTermSignature)  
                             extends XOwner(vars)
                                with ProxyTerm
{

  def proxy = p;

  override def isWith: Boolean = true;

  override def subst(s:PartialFunction[Term,Term])
                    (implicit ctx: CallContext) :
                                                  ComputationBounds[Term] = 
    ctx.withCall { 
       (ctx:CallContext) => implicit val ictx = ctx;
       val (newVars, s1) = copyVarFun(s);
       CallCC.compose(p.subst(s1),
              { (t:Term) => Done(new WithTerm(newVars,t,ws)) }
       );
    }

  override def print(out:PrintWriter):Unit = {
    out.print("with (");
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
    p.print(out);
  }

  override def signature = ws.apply(p.signature);

  override val attributes=new MutableHashMap[Name,Term]();
       
  private lazy val hash: Int = p.hashCode;

}

object WithTerm
{

  def unapply(t:WithTerm):Option[Tuple3[IndexedSeq[XTerm],Term,WithTermSignature]] =
         Some(t.vars, t.proxy, t.ws);

  def build(withFun: Term, theory: Theory):Term =
  {
    val theory = withFun.signature.theory;
    import theory._;
    withFun match {
      case Term(With,Seq(vardefs,body),_) => build(vardefs, body, theory)
    }
  }


  def build(vardefs: Term, body: Term, theory: Theory): Term =
  {
    theory.withSignature.createWithTerm(vardefs, body);
  }

  def transform(bindings: IndexedSeq[XTerm],
                bindingNames: Map[Name,Int],
                internalTerm: Term
                )(implicit ctx:CallContext):ComputationBounds[Term] =
  {
   val theory = internalTerm.signature.theory;
   import theory._; 
   internalTerm match {
     case AtomTerm(name,signature) =>
              bindingNames.get(name) match {
                 case Some(i) =>  Done(bindings(i))
                 case None   =>  Done(internalTerm)
              }
     case FunctionalTerm(With,Seq(vars,term),signature) =>
              // at first -- transform internal with
              transform(bindings, bindingNames,
                        build(vars,term,signature.theory)
                        )
     case FunctionalTerm(Let,Seq(assignments,term),signature) =>
              transform(bindings, bindingNames,
                        LetTerm.build(assignments,term,signature.theory)
                        )
     case FunctionalTerm(Eta,Seq(vars,ruleTerm),_) =>
                 transform(bindings, bindingNames,
                           EtaTerm.build(vars, ruleTerm, theory)
                          )
     case WithTerm(bindings1, body1, signature1) =>
                 CallCC.compose(
                     transform(bindings, bindingNames, body1),
                     (t:Term) => Done(new WithTerm(bindings1,t,signature1))
                 )
     case LetTerm(bindings1, body1, signature1) =>
                 CallCC.compose(
                     transform(bindings, bindingNames, body1),
                     (t:Term) => Done(new LetTerm(bindings1,t,
                                          TermConstructorTransformParams(false),
                                          signature1))
                 )
     case FunctionalTerm(name,subterms,signature) =>
                 ctx.withCall{ (ctx:CallContext) => implicit val ictx = ctx;
                               val transformed = subterms.map {
                                        t => try {
                                               transform(bindings, bindingNames,
                                                         t)
                                             } catch {
                                               case ex: CallCCThrowable[_] if ex.aManifest <:< manifest[Term] =>
                                                                ex.current.asInstanceOf[ComputationBounds[Term]]
                                             }
                                      }
                                signature.createTerm(name, transformed);
                             }
     case x  => Done(x)
   }
  }

}


