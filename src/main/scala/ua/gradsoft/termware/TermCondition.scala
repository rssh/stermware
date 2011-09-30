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
  def eval(s:STMSubstitution[Term]): (Boolean, STMSubstitution[Term])

  /**
   * logical and 
   **/
  def && (c: TermCondition) = AndTermCondition(this,c);

  @inline
  final def and (c: TermCondition) = &&(c);

  def || (c: TermCondition) = OrTermCondition(this,c);

  def unary_! = NotTermCondition(this);

}

object TrueTermCondition extends TermCondition
{

  def isQuickTrue=true;
  def isQuickFalse=false;
  def eval(s:STMSubstitution[Term]) = (true, s);

}

object FalseTermCondition extends TermCondition
{

  def isQuickTrue=false;
  def isQuickFalse=true;
  def eval(s:STMSubstitution[Term]) = (false, s);

}

case class AndTermCondition(val frs:TermCondition, val snd:TermCondition) extends TermCondition
{

  def isQuickTrue=(frs.isQuickTrue && snd.isQuickTrue);
  def isQuickFalse=(frs.isQuickFalse || snd.isQuickFalse);

  def eval(s:STMSubstitution[Term]) = 
    frs.eval(s) match {
      case (true,s1) => snd.eval(s1)
      case (false,s1) => (false,s)
    }

}

case class OrTermCondition(val frs:TermCondition, val snd:TermCondition) extends TermCondition
{

  def isQuickTrue=(frs.isQuickTrue || snd.isQuickTrue);
  def isQuickFalse=(frs.isQuickFalse && snd.isQuickFalse);

  def eval(s:STMSubstitution[Term]) = 
    frs.eval(s) match {
      case (true,s1) => (true, s1)
      case (false,s1) => snd.eval(s)
    }

}

case class NotTermCondition(val frs:TermCondition) extends TermCondition
{

  def isQuickTrue=(frs.isQuickFalse);
  def isQuickFalse=(frs.isQuickTrue);

  def eval(s:STMSubstitution[Term]) = 
    frs.eval(s) match {
      case (true, s1) => (false, s1)
      case (false, s1) => (true, s1)
    }

}

class FunTermCondition(
                        val fun: (Seq[Term]=>Boolean),
                        val args:Seq[Term]
                       ) extends TermCondition
{

  def isQuickTrue=false;
  def isQuickFalse=false;
   
  def eval(s:STMSubstitution[Term]) =
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
