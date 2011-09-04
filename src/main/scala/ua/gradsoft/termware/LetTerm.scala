package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;
import java.io.PrintWriter;
import scala.collection.mutable.{HashMap => MutableHashMap};

/**
 * term, which store bindings.
 **/
class LetTerm(val vars:IndexedSeq[TermBinding], 
              val p:Term, 
              val ls: LetTermSignature)  
                             extends ProxyTerm
{

  def proxy = p;

  override def subst(s:PartialFunction[Term,Term])
                    (implicit ctx: CallContext) :
                                           ComputationBounds[Term] = 
    ctx.withCall { 
       (ctx:CallContext) => implicit val ictx = ctx;
       val newVars = vars.map(_.subst(s)(ctx));
       val s1 = bindingSubstitution(newVars).andThen(s);
       // todo: refresh let in all subterms of p.
       CallCC.compose(p.subst(s1),
              { (t:Term) => Done(
                             new LetTerm(newVars,t,ls)
                            ) }
       );
    }


  override def print(out:PrintWriter):Unit = {
    out.print("let (");
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

  def bindingSubstitution(newVars:IndexedSeq[TermBinding]):PartialFunction[Term,Term]={
    new PartialFunction[Term,Term]{

       def isDefinedAt(t:Term):Boolean =
           t match {
              case x:LetProxy =>
                  (x.letOwner eq LetTerm.this)
              case _ =>
                    false;
           }

       def apply(t:Term):Term =
       {
         val lt = t.asInstanceOf[LetProxy];
         LetProxy(lt.name,lt.letLabel,null);
       }

    }
  }

  override def signature = ls.apply(p.signature);

  override val attributes=new MutableHashMap[Name,Term]();

  private lazy val hash: Int = p.hashCode;

}
