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

  /**
   * direct creation of eta-expression is not allowed.
   **/
  override def createTerm(name:Name, args: IndexedSeq[Term]):Term = 
       throwUOE;

  /**
   * v:IndexedSeq[XTerm], l:Term, r:Term, rs:Term
   **/
  override def createSpecial(args: Any*):Term = 
  {
    val v:IndexedSeq[XTerm]=args(0) match {
                     case x:IndexedSeq[XTerm] => x
                     case _                   => null
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
    return new EtaTerm(v,l,r,(if (rs==null) None else Some(rs)),this);
  }

  override def createConstant(arg:Any) = throwUOE; 

  def termType(t:Term): Term = {
     if (t.isEta) {
       val lt:Term = t.subterm(0).termType;
       val rt:Term = t.subterm(1).termType;
       val rst: Term = t.subterm(2).termType;
       val tct= theory.freeFunSignature.createTerm("OR",
                  theory.freeFunSignature.createTerm("ARROW",lt,rt),
                  rst);
       theory.typeAlgebra.reduce(tct)._1;
     } else {
       throwUOE;
     }
  }


  val theory=th;

}

