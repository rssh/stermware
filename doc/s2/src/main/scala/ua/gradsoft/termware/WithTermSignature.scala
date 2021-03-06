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
       throw new IllegalArgumentException("name of with-term must be 'with', have "+name);
    }
    if (args.length!=2) {
       throw new IllegalArgumentException("args lenght must be 2");
    }
    createWithTerm(args(0), args(1));
  }

  private[termware] def createWithTerm(vardefs: Term, body: Term):Term =
  {
    val vNames:(IndexedSeq[XTerm],Map[Name,Int]) = parseNames(vardefs);
    if (vNames._1.isEmpty) {
       body
    } else {
       new WithTerm(vNames._1,
             CallCC.trampoline(
                Call{ (ctx:CallContext) =>   WithTerm.transform(vNames._1,vNames._2,body)(ctx) }
             ),
             this
       );
    }
  }

  /**
   * v:IndexedSeq[XTerm], p:Term,
   **/
  override def createSpecial(args: Any*):Term = 
  {
   if (args(0).isInstanceOf[IndexedSeq[_]]
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

  def toAnyRef(t:Term) =
     t match {
       case pt: WithTerm => pt.proxy.toAnyRef;
       case _ => t
     }


  def toAny(t:Term) = 
     t match {
       case pt: WithTerm => pt.proxy.toAny;
       case _ => t
     }

  /**
   * we can;t create with from external term,
   * except it was selt.
   **/
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

  def apply(s:TermSignature):TermSignature = s;

  val theory=th;


  
  /**
   * accep list of 'vardef'
   **/
  private def parseNames(t:Term):(IndexedSeq[XTerm],Map[Name,Int]) = {
      var m = Map[Name,Int]();
      val vardefName = theory.symbolTable.getOrCreate("vardef");
      var i=0;
      var seq = indexedSeqFromTermList(theory, t) map { (t:Term)=>
             if (t.name == vardefName) {
                 val varName = t.subterm(0);
                 val varType = t.subterm(1);
                 val x = new XTerm(varName.name,i,varType,null,theory.xSignature);
                 m=m.updated(varName.name,i);
                 i = i+1;
                 x;
             } else {
                 throw new IllegalArgumentException("vardef required, have "+t.name);
             }
      };
      (seq,m);
  }

  def to[T](t:Term)(implicit mt:Manifest[T]):Option[T] = None;
  
  def from[T](x:T)(implicit mt:Manifest[T]):Option[Term] = None;
  


}

