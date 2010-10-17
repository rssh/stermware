package ua.gradsoft.termware;

import scala.collection.Set;
import scala.collection.immutable.Map;
import scala.collection.immutable.TreeMap;
import scala.collection.mutable.HashMap;
import java.io.PrintWriter;
import ua.gradsoft.termware.fn._;
import ua.gradsoft.termware.util._;


class EtaTerm(v:Set[EtaXTerm], l:Term, r:Term, rs:Term, s:EtaTermSignature)  
                             extends EtaXOwner
                                         with ComplexUnify
                                         with ComplexSubst
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
      case 1 => left
      case 2 => right
      case 3 => rest
      case _ => throwUOE
    }

  override def termUnifyFn(t:Term,s:Substitution) = 
    (vm: VM) => {
       if (t.isEta) {
         vm.pushCommandsReverse(
            left.termUnifyFn(t.subterm(0),s),
            (vm:VM) => {
               val b: Boolean = vm.popData.asInstanceOf[Boolean];
               if (b) {
                  val s1 = vm.popData.asInstanceOf[Substitution];
                  vm.pushCommandsReverse(
                     right.termUnifyFn(t.subterm(1),s1),
                     (vm:VM) => {
                       if (vm.popData.asInstanceOf[Boolean]) {
                         val s2 = vm.popData.asInstanceOf[Substitution];
                         vm.pushCommand(
                            rest.termUnifyFn(t.subterm(2),s2)
                         );
                       } else {
                         vm.pushData(false);
                       };
                       vm;
                     }
                  );
               } else {
                  vm.pushData(false);
               }
               vm;
            }
         );
       }else{
         vm.pushData(false);
       };
       vm;
    }


  override def termSubstFn(s:PartialFunction[Term,Term]):VM=>VM = 
    (vm:VM) => {
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
       vm.pushCommandsReverse(
          rest.termSubstFn(s1),
          right.termSubstFn(s1),
          left.termSubstFn(s1),
          (vm:VM) => {
              val nLeft = vm.popData.asInstanceOf[Term];
              val nRight = vm.popData.asInstanceOf[Term];
              val nRest = vm.popData.asInstanceOf[Term];
              val retval=new EtaTerm(vars,nLeft,nRight,nLeft,signature);
              vm.pushData(retval);
              vm
          }
       );
       vm;
    }

  override def termClassIndex  = TermClassIndex.ETA;

  override def termHashCode = hashCode;

  override def termCompare(t:Term):Int = {
     var c = termClassIndex - t.termClassIndex;
     if (c!=0) {
        return c;
     } else {
        c = left.termCompare(t.subterm(0));
        if (c!=0) return c; 
        c = right.termCompare(t.subterm(1));
        if (c!=0) return c; 
        c = rs.termCompare(t.subterm(2));
        return c;
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

  var attributes=new HashMap[Name,Term]();

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
