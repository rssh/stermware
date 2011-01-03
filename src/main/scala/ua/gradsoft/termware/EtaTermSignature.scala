package ua.gradsoft.termware;

import scala.collection.immutable.TreeSet;
import ua.gradsoft.termware.flow._;
import ua.gradsoft.termware.util.TermOrdering;


class EtaTermSignature(th:Theory) extends TermSignature
                                    with GeneralUtil
{

  override def fixedName = Some(theory.symbolTable.ETA);

  override def fixedArity = Some(0);

  override def nameByIndex = None;
  override def indexByName = None;

  override def createTerm(name:Name, args: IndexedSeq[Term]):Term = {
    if (args.length!=3) {
       throwUOE;
    }
    val v:Set[EtaXTerm]=collectEtaX(args:_*);
    return new EtaTerm(v,args(0),args(1),args(2), this);
  }

  /**
   * v:Set[EtaXTerm], l:Term, r:Term, rs:Term
   **/
  override def createSpecial(args: Any*):Term = 
  {
    val v:Set[EtaXTerm]=args(0) match {
                     case x:Set[EtaXTerm] => x
                     case _               => null
    };
    if (v==null) throwUOE;
    val l:Term = args(1) match {
               case t:Term => t
               case _      => null
    };
    if (l==null) throwUOE;
    val r:Term = args(2) match {
               case t:Term => t
               case _      => null
    };
    if (r==null) throwUOE;
    val rs:Term = args(3) match {
               case t:Term => t
               case _      => null
    };
    if (rs==null) throwUOE;
    return new EtaTerm(v,l,r,rs,this);
  }

  override def createConstant(arg:Any) = throwUOE; 

  def termType(ct:ComputationBounds[Term])(implicit ctx:CallContext):
                                              ComputationBounds[Term] = {
    if (ct.isDone) {
     val t = ct.result.get;
     if (t.isEta) {
       val lct:ComputationBounds[Term] = t.subterm(0).termType;
       val rct:ComputationBounds[Term] = t.subterm(1).termType;
       val rsct: ComputationBounds[Term] = t.subterm(2).termType;
       val tct= theory.freeFunSignature.createTerm("OR",
                  theory.freeFunSignature.createTerm("ARROW",lct,rct),
                  rsct);
       theory.typeAlgebra.reduce(tct);
     } else {
       throwUOE;
     }
   } else {
     CallCC.compose(ct,{ 
       (t:Term,ctx:CallContext) => termType(Done(t))(ctx);
     });
   }
  }

  private def collectEtaX(t:Term):Set[EtaXTerm] = {
     var r = TreeSet.empty[EtaXTerm](etaXTermOrdering);
     for(st <- t.subterms) {
       st match {
         case x: EtaXTerm => {
           if (x.xOwner eq null) {
              r=r+x;
           }else{
              /* do nothing */
           }
         }
         case _ => { if (st.arity>0) {
                       r=r++collectEtaX(st);
                     }  
         }
       }
     } 
     return r;
  }


  private def collectEtaX(terms:Term*):Set[EtaXTerm] = {
     var r = TreeSet.empty[EtaXTerm](etaXTermOrdering);
     for(t <- terms) yield {
       r++=collectEtaX(t);
     }
     return r;
  }

  val theory=th;

  lazy val etaXTermOrdering=new TermOrdering[EtaXTerm]();
}

