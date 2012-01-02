package ua.gradsoft.termware;

import scala.collection.Set;
import scala.collection.immutable.Map;
import scala.collection.immutable.TreeMap;
import scala.collection.mutable.{HashMap => MutableHashMap};
import java.io.PrintWriter;
import ua.gradsoft.termware.util._;
import ua.gradsoft.termware.flow._;


class WithTerm(vars:IndexedSeq[XTerm], val p:Term, ws: WithTermSignature)  
                             extends XOwner(vars)
                                with ProxyTerm
{

  def proxy = p;

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

  def build(vardefs:Term, main: Term, theory: Theory):WithTerm =
  {
   //TODO: implement
   throw new RuntimeException("not implemented");
  }

}
