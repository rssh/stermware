package ua.gradsoft.termware;

import scala.collection.immutable._;
import ua.gradsoft.termware.fn._;
import ua.gradsoft.termware.util.TermOrdering;


class EtaTermSignature(th:Theory) extends TermSignature
                                    with GeneralUtil
{

  override def fixedName = Some(theory.symbolTable.ETA);

  override def fixedArity = Some(0);

  override def nameByIndex = None;
  override def indexByName = None;

  override def createTerm(name:Name, args:RandomAccessSeq[Term]):Term = {
    if (args.length!=3) {
       throwUOE;
    }
    val v:Set[EtaXTerm]=collectEtaX(args:_*);
    return new EtaTerm(v,args(0),args(1),args(2), this);
  }

  /**
   * v:Set[EtaXTerm], l:Term, r:Term, rs:Term
   **/
  override def createSpecial(args: Any*):Term = {
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


  def getTypeFn(t:Term):VM=>VM = {
    var retval=t.getAttribute(theory.symbolTable.TYPE);
    if (retval==None) {
      return (vm:VM) => {
            vm.pushCommandsReverse(
             calculateTypeFn(t),
             DupData,
             SetAttributeFnTN_V(t,theory.symbolTable.TYPE)
            );
            vm;
      };
    } else {
       return PushData(retval.get);
    }
  }

  def calculateTypeFn(t:Term):VM=>VM = {
     vm:VM => {
       val left = t.subterm(0);
       val right = t.subterm(1);
       val rest = t.subterm(2);
       vm.pushCommandsReverse(
          left.signature.getTypeFn,
          ToData2,
          right.signature.getTypeFn,
          ToData2,
          FromData2,
          FromData2,
	  theory.freeFunSignature.createTermFn("FUN",2),
          rest.signature.getTypeFn,
	  theory.freeFunSignature.createTermFn("OR",2),
          theory.typeAlgebra.reduceFn
       );
       vm;
     }
  }

  def getType(t:Term):Term = {
    var retval=t.getAttribute(theory.symbolTable.TYPE);
    if (retval==None) {
        retval=Some(calculateType(t));
        t.setAttribute(theory.symbolTable.TYPE,retval.get);
    }
    return retval.get;
  }

  def calculateType(t:Term):Term = {
     val left = t.subterm(0);
     val leftType = left.signature.getType(left);
     val right = t.subterm(1);
     val rightType = right.signature.getType(right);
     val rest = t.subterm(2);
     val restType = rest.signature.getType(rest);
     val etaTypeIn = theory.freeFunSignature.createTerm(
                             "OR",
                             theory.freeFunSignature.createTerm("ARROW",
                                                              leftType,
                                                              rightType),
                             restType);
     return theory.typeAlgebra.reduce(etaTypeIn);
  }
  
  implicit def toTermOrdering[A<:Term] = new TermOrdering[A];

  private def collectEtaX(t:Term):Set[EtaXTerm] = {
     var r = TreeSet.empty[EtaXTerm];
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
     var r = TreeSet.empty[EtaXTerm];
     for(t <- terms) yield {
       r++=collectEtaX(t);
     }
     return r;
  }


  val theory=th;
}

