package ua.gradsoft.termware;

import scala.collection.immutable.TreeSet;
import ua.gradsoft.termware.flow._;
import ua.gradsoft.termware.util.TermOrdering;


class LetTermSignature(th:Theory) extends TermSignature
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
    if (args.length!=3) {
       throw new IllegalArgumentException("args lenght must be 3");
    }
    val vNames:(IndexedSeq[TermBinding],Map[Name,Int]) = parseNames(args(0));
    val t = args(1);
    if (vNames._1.isEmpty) {
       t;   
    } else {
        val s = new PartialFunction[Term,Term]{
           def isDefinedAt(x:Term) = x.isAtom;
           def apply(t:Term):Term = vNames._2.get(t.name) match {
                                      case Some(x) => LetProxy(t.name,x,null)
                                      case None    => t
                                    };
        };
        new LetTerm(vNames._1,t.fixSubst(s),this);
    }
  }

  def termType(ct:ComputationBounds[Term])(implicit ctx:CallContext):
                                              ComputationBounds[Term] = {
     if (ct.isDone) {
       ct.result.get.signature.termType(ct);
     } else {
       CallCC.compose(ct,(t:Term)=>t.signature.termType(Done(t)) );
     }
  }


  def apply(s:TermSignature):TermSignature = s;

  val theory=th;

  
  /**
   * accep list of 'assignments'
   **/
  private def parseNames(t:Term):(IndexedSeq[TermBinding],Map[Name,Int]) = {
      var m = Map[Name,Int]();
      val assignName = theory.symbolTable.getOrCreate("assign");
      var i=0;
      var seq = indexedSeqFromTermList(theory, t) map { (x:Term)=>
             if (t.name == assignName) {
                 val letName = t.subterm(0).name;
                 val letKind = t.subterm(1);
                 val letValue = t.subterm(2);
                 val binding = TermBinding(letName,letKind.getInt,Done(letValue));
                 m=m.updated(letName,i);
                 i = i+1;
                 binding;
             } else {
                 throw new IllegalArgumentException("assign required");
             }
      };
      (seq,m);
  }

  override def createConstant(arg:Any) = throwUOE;

  override def createSpecial(args: Any*) = throwUOE;

}

