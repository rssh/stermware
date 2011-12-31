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

object LetTerm
{

   /**
    * build let term from 'let' functional term, readed by parser.
    * on error during attempt to build let term throw IllegalArgumentException
    **/
   def build(letFun:Term): LetTerm =
   {
     val theory = letFun.signature.theory;
     import theory._;
     letFun match {
        case Term(Let, IndexedSeq(assigments,body), _) => build(assigments,body,theory)
        case Term(Where, IndexedSeq(assigments,body),_) => build(assigments,body,theory)
     }
   }

   /**
    * build let term from 'let' functional term, readed by parser.
    * we can look on it as on 'let-0rransform'
    *   let (assigments) z = let-transform(binging, z)
    *   let-transform(binding, f(x1,..xN)) = f(let-transform(x1),...let-transform(xN))
    *   let-transform(binding, name) = binding(name) if name in binding 
    *                                          name  otherwise.
    *   let-transform(binding, let(assigments1, name)) = let-transform(binding,let-transform(binding1, name))
    *   let-transform(binding, eta(vars, z)) = let-transform(binding,eta-transform(vars, z))
    *   let-transform(binding, with(vars, z)) = let-transform(binding,with-transform(vars, z))
    **/
   def build(assigments:Term, main:Term, theory: Theory): LetTerm =
   {
     //TODO: implement
     throw new RuntimeException("not implemented");
   }


}

