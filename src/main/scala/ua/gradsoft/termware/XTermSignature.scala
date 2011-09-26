package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

class XTermSignature(th:Theory) extends TermSignature
                                       with GeneralUtil
{

  override def fixedName = None;

  override def fixedArity = Some(0);

  override def nameByIndex = None;
  override def indexByName = None;

  override def createTerm(name:Name, args:IndexedSeq[Term]):Term = throwUOE;

  /**
   * n:Name, i:Int, t:Term, (type)
   **/
  override def createSpecial(args: Any*):Term = {
    val n:Name = args(0) match {
           case x:Name => x
           case _      => null
    }
    if (n==null) throwUOE;
    val i:Int = args(1) match {
            case i:Int => i
            case _     => -1;
    }
    if (i == -1) throwUOE;
    val t:Term = args(2) match {
               case t:Term => t
               case _      => null
    };
    if (t==null) throwUOE;
    return new XTerm(n,i,t,null,this);
  }

  override def createConstant(arg:Any) = throwUOE; 

  def termType(t:Term) :Term = {
    t.getAttribute(theory.symbolTable.TYPE) match {
         case Some(x) => x
         case None => {
            val retval = theory.typeAlgebra.top;
            t.setAttribute(theory.symbolTable.TYPE, retval);
            retval;
         }
    }
  }

  val theory=th;
}

