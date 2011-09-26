package ua.gradsoft.termware;

import scala.collection.immutable.TreeSet;
import ua.gradsoft.termware.flow._;
import ua.gradsoft.termware.util.TermOrdering;


class WithTermSignature(th:Theory) extends TermSignature
                                    with TheoryTermConversions
                                    with GeneralUtil
{

  override def fixedName = None;

  override def fixedArity = None;

  override def nameByIndex = None;
  override def indexByName = None;

  /**
   * direct creation of with-expression is not allowed.
   **/
  override def createTerm(name:Name, args: IndexedSeq[Term]):Term = 
  {
    if (name != theory.symbolTable.WITH) {
       throw new IllegalArgumentException("name of with-term must be 'with'");
    }
    if (args.length!=2) {
       throw new IllegalArgumentException("args lenght must be 2");
    }
    val vNames:(IndexedSeq[XTerm],Map[Name,Int]) = parseNames(args(0));
    val t = args(1);
    if (vNames._1.isEmpty) {
       t;   
    } else {
        val s = new PartialFunction[Term,Term]{
           def isDefinedAt(x:Term) = x.isAtom;
           def apply(t:Term):Term = vNames._2.get(t.name) match {
                                      case Some(x) => vNames._1(x)
                                      case None    => t
                                    };
        };
        new WithTerm(vNames._1,t.fixSubst(s),this);
    }
  }

  /**
   * v:IndexedSeq[XTerm], p:Term,
   **/
  override def createSpecial(args: Any*):Term = 
  {
   if (args(0).isInstanceOf[IndexedSeq[XTerm]]
       &&
       args(1).isInstanceOf[Term]) {
     new WithTerm(args(0).asInstanceOf[IndexedSeq[XTerm]],
                  args(1).asInstanceOf[Term],
                  this);
   } else {
     throwUOE;
   }
  }


  override def createConstant(arg:Any) = throwUOE; 

  def termType(t:Term): Term = {
     t match {
       case pt: WithTerm => pt.proxy.termType
       case _  => throwUOE
     }
  }


  def apply(s:TermSignature):TermSignature = s;

  val theory=th;

  
  /**
   * accep list of 'vardef'
   **/
  private def parseNames(t:Term):(IndexedSeq[XTerm],Map[Name,Int]) = {
      var m = Map[Name,Int]();
      val vardefName = theory.symbolTable.getOrCreate("vardef");
      var i=0;
      var seq = indexedSeqFromTermList(theory, t) map { (x:Term)=>
             if (t.name == vardefName) {
                 val varName = t.subterm(0);
                 val varType = t.subterm(1);
                 val x = new XTerm(varName.name,i,varType,null,theory.xSignature);
                 m=m.updated(varName.name,i);
                 i = i+1;
                 x;
             } else {
                 throw new IllegalArgumentException("vardef required");
             }
      };
      (seq,m);
  }

}

