package ua.gradsoft.termware;

import scala.collection.Set;
import scala.collection.immutable.Map;
import scala.collection.immutable.TreeMap;
import scala.collection.mutable.HashMap;
import java.io.PrintWriter;
import ua.gradsoft.termware.fn._;
import ua.gradsoft.termware.util._;
import ua.gradsoft.termware.flow._;


class WithTerm(vars:IndexedSeq[XTerm], val p:Term, ws: WithTermSignature)  
                             extends XOwner(vars)
                                with ProxyTerm
                                         with ComplexUnify
                                         with ComplexSubst
                                         with ComplexCompare
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

  def signature = ws.apply(p.signature);

  var attributes=new HashMap[Name,ComputationBounds[Term]]();
       
  private lazy val hash: Int = p.hashCode;

}
