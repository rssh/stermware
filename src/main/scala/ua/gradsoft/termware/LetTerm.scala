package ua.gradsoft.termware;

import java.io.PrintWriter;
import scala.collection.mutable.{HashMap => MutableHashMap};
import scala.collection.immutable.TreeMap;
import ua.gradsoft.termware.flow._;

/**
 * term, which store bindings and let exression.
 * i. e. 
 * <pre>  
 *   let (x1 <- v1,... xN <- vN) p
 * </pre>
 *@param vars -- table of variables, which consists from index, name and actual term value.
 *@param iniBody -- term inside let expression. (where x1 ... xN - atoms with appropriative names).
 *@param transformParams -- operations, which must be done before first usage.
 *@param letSignature -- let signature.
 **/
class LetTerm(val vars:IndexedSeq[TermBinding], 
              iniBody:Term, 
              private val transformParams:TermConstructorTransformParams[LetTerm],
              val letSignature: LetTermSignature)  
                             extends ProxyTerm
{

  def proxy = letTransformed;

  override def subst(s:PartialFunction[Term,Term])
                    (implicit ctx: CallContext) :
                                           ComputationBounds[Term] = 
   ctx.withCall { 
       (ctx:CallContext) => implicit val ictx = ctx;
       val newVars = vars.map(_.subst(s)(ictx));
       Done( new LetTerm(newVars, proxy, 
                         TermConstructorTransformParams((s,this)),
                         letSignature) )
    }

  override def fixSubst(s: PartialFunction[Term,Term]): Term =
              CallCC.trampoline(Call{(ctx:CallContext)=>subst(s)(ctx)});


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
    proxy.print(out);
  }



  override def signature = letSignature.apply(letTransformed.signature);

  override lazy val attributes=new MutableHashMap[Name,Term]();

  private[this] lazy val hash: Int = letTransformed.hashCode;

  private[this] lazy val letTransformed = 
                  if (transformParams.isDoTransformation) {
                        CallCC.trampoline(
                          Call{ (ctx:CallContext) => 
                                 val retval = LetTerm.transform(vars,iniBody,this)(ctx); 
                                 transformParams.releaseRefs;
                                 retval;
                          }
                         );
                  } else if (transformParams.substParams!=None) {
                      val substParams = transformParams.substParams.get;
                      transformParams.releaseRefs;
                      CallCC.trampoline(
                         Call{ (ctx:CallContext) =>
		           iniBody.subst(LetTerm.bindingSubstitution(vars,substParams._1,
                                                                          substParams._2, this))(ctx);
                         }
                      );
                  } else {
                      iniBody;
                  }

}

object LetTerm
{

   /**
    * build let term from 'let' functional term, readed by parser.
    * on error during attempt to build let term throw IllegalArgumentException
    **/
   def build(letFun:Term): Term =
   {
     val theory = letFun.signature.theory;
     import theory._;
     letFun match {
        case Term(Let, Seq(assigments,body), _) => build(assigments,body,theory)
        case Term(Where, Seq(assigments,body),_) => build(assigments,body,theory)
     }
   }

   def unapply(letTerm:LetTerm):Option[Tuple3[IndexedSeq[TermBinding],Term,LetTermSignature]] =
    Some((letTerm.vars,letTerm.proxy,letTerm.letSignature));

  

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
   def transform(bindings:IndexedSeq[TermBinding],
                 internalTerm: Term,
                 owner: LetTerm)(implicit ctx:CallContext):ComputationBounds[Term] = 
      transform(bindings, bindingNames(bindings), internalTerm, owner)(ctx);
    
   def transform(bindings:IndexedSeq[TermBinding],
                 bindingNames:Map[Name,Int],
                 internalTerm:Term,
                 owner:LetTerm)(implicit ctx:CallContext):ComputationBounds[Term] =
   {
     val theory = internalTerm.signature.theory;
     import theory._;
     internalTerm match {
        case AtomTerm(name,signature) => 
                 bindingNames.get(name) match {
                    case Some(i) => Done(new LetProxy(name,i,owner))
                    case None  => Done(internalTerm)
                 }
        case LetTerm(bindings1,body1,letSignature1) => 
                 CallCC.compose( 
                      transform(bindings, bindingNames, body1, owner),
                      { (t:Term) => 
                        Done(new LetTerm(bindings1,
                                      t, 
                                      TermConstructorTransformParams((Map[Term,Term](),
                                                                     // TODO: rethink
                                                                     internalTerm.asInstanceOf[LetTerm])),
                                      letSignature1))
                      }
                  );
        case FunctionalTerm(Let,Seq(assignments,term),_) =>
                 // at first crealte LetTerm inside.
                 transform(bindings, bindingNames, 
                           build(assignments, term, theory),
                           owner)
        case FunctionalTerm(With,Seq(vars,term),_) =>
                 transform(bindings, bindingNames, 
                           WithTerm.build(vars, term, theory),
                           owner)
        case FunctionalTerm(Eta,Seq(vars,ruleTerm),_) =>
                 transform(bindings, bindingNames, 
                           EtaTerm.build(vars, ruleTerm, theory),
                           owner)
        case FunctionalTerm(name,subterms,signature) =>
                           ctx.withCall { (ctx:CallContext) => implicit val ictx = ctx;
                             val transformed = subterms.map {
                                (x:Term) => try {
                                             transform(bindings, bindingNames,
                                                     x, owner);
                                            } catch {
                                             case ex: CallCCThrowable[_] if ex.aManifest <:< manifest[Term] =>
                                                            ex.current.asInstanceOf[ComputationBounds[Term]]
                                            }
                             }
                             signature.createTerm(name,transformed)
                           }
        case x => Done(x)
     }
   }

   def bindingNames(bindings:IndexedSeq[TermBinding]):Map[Name,Int] =
   {
    var retval = TreeMap[Name,Int]();
    for(i <- 0 until bindings.length) {
        retval = retval.updated(bindings(i).name,i);
    }
    retval;
   }


   def build(assignments:Term, main:Term, theory: Theory): Term =
   {
     theory.letSignature.createTerm(theory.symbolTable.LET, assignments, main);
   }

   def bindingSubstitution(newVars:IndexedSeq[TermBinding], 
                           nextSubst: PartialFunction[Term,Term],
                           oldOwner: LetTerm,  newOwner: LetTerm):PartialFunction[Term,Term]={
    new PartialFunction[Term,Term]{
       def isDefinedAt(t:Term):Boolean =
           t match {
              case x:LetProxy =>
                  (x.letOwner eq oldOwner)
              case _ => nextSubst.isDefinedAt(t)
           }
       def apply(t:Term):Term =
       {
         t match {
              case x:LetProxy if (x.letOwner eq oldOwner) =>
                    LetProxy(x.letName,x.letLabel, newOwner);
              case _ => if (nextSubst.isDefinedAt(t)) 
                           nextSubst(t)
                        else t
         }
       }
    }
  }

}

