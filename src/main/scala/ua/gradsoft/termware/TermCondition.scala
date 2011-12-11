package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

trait TermCondition 
{

  /**
   * can we quickly say, that this condition is evaluated to false
   * without futer evaluation
   **/
  def isQuickFalse: Boolean


  /**
   * can we quickly say, that this condition is evaluated to true
   * without future evaluation
   **/
  def isQuickTrue:  Boolean;
  
  /**
   * eval condition with given substitution
   **/
  def eval(s:Substitution[Term]): (Boolean, Substitution[Term])

  /**
   * logical and 
   **/
  def && (c: TermCondition) = AndTermCondition(this,c);

  @inline
  final def and (c: TermCondition) = &&(c);

  /**
   * logical or
   **/
  def || (c: TermCondition) = OrTermCondition(this,c);

  @inline
  final def or (c: TermCondition) = ||(c);

  def unary_! = NotTermCondition(this);


}

object TrueTermCondition extends TermCondition
{

  def isQuickTrue=true;
  def isQuickFalse=false;
  def eval(s:Substitution[Term]) = (true, s);

}

object FalseTermCondition extends TermCondition
{

  def isQuickTrue=false;
  def isQuickFalse=true;
  def eval(s:Substitution[Term]) = (false, s);

}

case class AndTermCondition(val frs:TermCondition, val snd:TermCondition) extends TermCondition
{

  def isQuickTrue=(frs.isQuickTrue && snd.isQuickTrue);
  def isQuickFalse=(frs.isQuickFalse || snd.isQuickFalse);

  def eval(s:Substitution[Term]) = 
    frs.eval(s) match {
      case (true,s1) => snd.eval(s1)
      case (false,s1) => (false,s)
    }

}

case class OrTermCondition(val frs:TermCondition, val snd:TermCondition) extends TermCondition
{

  def isQuickTrue=(frs.isQuickTrue || snd.isQuickTrue);
  def isQuickFalse=(frs.isQuickFalse && snd.isQuickFalse);

  def eval(s:Substitution[Term]) = 
    frs.eval(s) match {
      case (true,s1) => (true, s1)
      case (false,s1) => snd.eval(s)
    }

}

abstract class CbTermCondition extends TermCondition
{
  def isQuickTrue=false;
  def isQuickFalse=false;

  def cbEval(s:Substitution[Term])(implicit ctx:CallContext):
             ComputationBounds[(Boolean,Substitution[Term])]

  def eval(s:Substitution[Term])=
     CallCC.trampoline(Call{(ctx:CallContext)=>cbEval(s)(ctx)});

}

case class EqTermCondition(val frs:Term, val snd:Term) extends CbTermCondition
{
  override def isQuickTrue=false;
  override def isQuickFalse=false;

  def cbEval(s:Substitution[Term])(implicit ctx:CallContext) = 
  {
    import CallCC._;
    pair(
          compose(pair(frs.subst(s),snd.subst(s)),
              { (xy:Pair[Term,Term],ctx:CallContext) => implicit val ictx = ctx;
                                                        xy._1.termEq(xy._2) }),
          Done(s)
    );
  }

}

case class NotTermCondition(val frs:TermCondition) extends TermCondition
{

  def isQuickTrue=(frs.isQuickFalse);
  def isQuickFalse=(frs.isQuickTrue);

  def eval(s:Substitution[Term]) = 
    frs.eval(s) match {
      case (true, s1) => (false, s1)
      case (false, s1) => (true, s1)
    }

}

class FunSeqTermCondition(
                        val fun: (Seq[Term]=>Boolean),
                        val args:Seq[Term]
                       ) extends TermCondition
{

  def isQuickTrue=false;
  def isQuickFalse=false;
   
  def eval(s:Substitution[Term]) =
                (fun(args.map(_.fixSubst(s))),s)

}


object TermCondition
{

   def apply(v:Boolean):TermCondition =
    v match {
       case true => TrueTermCondition
       case false => FalseTermCondition
    }

   def apply(t:Term, ts:TermSystem):TermCondition = build(t,ts);

   def build(t:Term, ts:TermSystem):TermCondition =
   {
     if (t.isBoolean) {
         apply(t.getBoolean);
     } else {
         //TODO: implement
         throw new UnsupportedOperationException("not implemented");
     }
   }


}
