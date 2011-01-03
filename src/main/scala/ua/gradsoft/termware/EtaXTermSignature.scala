package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

class EtaXTermSignature(th:Theory) extends TermSignature
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
    return new EtaXTerm(n,i,t,null,this);
  }

  override def createConstant(arg:Any) = throwUOE; 

  def termType(ct:ComputationBounds[Term])(implicit ctx:CallContext)
                                                  :ComputationBounds[Term] = {
   if (ct.isDone) {
    val t = ct.result.get;
    t.getAttribute(theory.symbolTable.TYPE) match {
         case None => {
            val retval = Done(theory.typeAlgebra.top)
            t.setAttribute(theory.symbolTable.TYPE, retval);
            retval;
         }
         case Some(x) => x
    }
   } else {
    CallCC.compose(ct, { (x:Term, ctx:CallContext) => termType(Done(x))(ctx); })
   }
  }

  val theory=th;
}

