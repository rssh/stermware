package ua.gradsoft.termware;

import ua.gradsoft.termware.fn._;

class EtaXTermSignature(th:Theory) extends TermSignature
                                       with GeneralUtil
{

  override def fixedName = None;

  override def fixedArity = Some(0);

  override def nameByIndex = None;
  override def indexByName = None;

  override def createTerm(name:Name, args:RandomAccessSeq[Term]):Term = throwUOE;

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
    return new EtaXTerm(n,i,t,null,this);
  }

  override def createConstant(arg:Any) = throwUOE; 

  def getTypeFn(t:Term):VM=>VM = {
    // TODO: parametrize Signature by subtype of term
    var retval=t.getAttribute(theory.symbolTable.TYPE);
    if (retval==None) {
      return PushData(theory.typeAlgebra.top);
    } else {
      return PushData(retval.get);
    }
  }

  def getType(t:Term):Term = {
    var retval=t.getAttribute(theory.symbolTable.TYPE);
    if (retval==None) {
        retval=Some(theory.typeAlgebra.top);
        t.setAttribute(theory.symbolTable.TYPE,retval.get);
    }
    return retval.get;
  }

  val theory=th;
}

