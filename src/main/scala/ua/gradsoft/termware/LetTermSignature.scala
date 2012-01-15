package ua.gradsoft.termware;

import scala.collection.immutable.TreeSet;
import ua.gradsoft.termware.flow._;
import ua.gradsoft.termware.util.TermOrdering;


class LetTermSignature(val theory:Theory) extends TermSignature
                                    with TheoryTermConversions
                                    with GeneralUtil
{

  override def fixedName = None;

  override def fixedArity = None;

  override def nameByIndex = None;
  override def indexByName = None;

  /**
   * direct creation of let-expression is not allowed.
   **/
  override def createTerm(name:Name, args: IndexedSeq[Term]):Term = 
  {
    if (name != theory.symbolTable.LET) {
       throw new IllegalArgumentException("name of let-term must be 'let'");
    }
    if (args.length!=2) {
       throw new IllegalArgumentException("args length must be 2");
    }
    val bindings:IndexedSeq[TermBinding] = parseAssignments(args(0));
    val t = args(1);
    if (bindings.isEmpty) {
       t;   
    } else {
       new LetTerm(bindings,t,TermConstructorTransformParams(true),this);
    }
  }

  def termType(t:Term):Term = {
    t match {
      case pt:ProxyTerm => pt.proxy.termType
      case _ => throwUOE
    }
  }


  def apply(s:TermSignature):TermSignature = s;

  /**
   * accep list of 'assignments'
   **/
  private def parseAssignments(t:Term):IndexedSeq[TermBinding] = {
      val assignName = theory.symbolTable.getOrCreate("assign");
      var i=0;
      var seq = indexedSeqFromTermList(theory, t) map { (x:Term)=>
             if (x.name == assignName) {
                 val letName = x.subterm(0).name;
                 val letKind = x.subterm(1);
                 val letValue = x.subterm(2);
                 val binding = TermBinding(letName,letKind.getInt,letValue);
                 i = i+1;
                 binding;
             } else {
                 throw new IllegalArgumentException("assign required");
             }
      };
      seq;
  }

  override def createConstant(arg:Any) = throwUOE;

  override def createSpecial(args: Any*) = throwUOE;

   /**
    * t when unbound, or proxy.getAnyRef when bound.
    **/
   def toAnyRef(t:Term) = 
     t match {
       case pt: ProxyTerm => pt.proxy.toAnyRef
       case _ => t
     }

  def toAny(t:Term) = 
     t match {
       case pt: ProxyTerm => pt.proxy.toAnyRef
       case _ => t
     }

  def fromAnyRef(x:AnyRef) =
    x match {
      case t: Term => Some(t) 
      case _ => None
    }

  def fromAny(x:Any) =
    x match {
      case r: AnyRef => fromAnyRef(r)
      case _ => None
    }

  def to[T](t:Term)(implicit mt:Manifest[T]):Option[T] = None;
  
  def from[T](x:T)(implicit mt:Manifest[T]):Option[Term] = None;


}

