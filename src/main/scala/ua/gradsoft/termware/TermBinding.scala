package ua.gradsoft.termware;

import java.io.PrintWriter;
import ua.gradsoft.termware.flow._;

case class TermBinding(val name: Name, val kind:Int, var value: Term)
{

  def subst(s:PartialFunction[Term,Term])(implicit ctx:CallContext)
                                                         :TermBinding =
    kind match {
     case TermBinding.EAGER =>
        TermBinding(name, kind, ComputationBoundsTerm(value.subst(s)(ctx)))
     case TermBinding.LAZY =>
        TermBinding(name, kind, ComputationBoundsTerm(
                          Call{ (ctx:CallContext) => value.subst(s)(ctx) }
                                                     ))
    }


  def print(out:PrintWriter):Unit = {
     out.print(name);
     out.print(" ");
     out.print(arrow);
     out.print(" ");
     out.print(value);
  }

  def arrow: String = 
    kind match {
      case TermBinding.EAGER => "<-*"
      case TermBinding.LAZY =>  "<-:"
    }

}

object TermBinding
{
  val EAGER = 1;
  val LAZY = 2;
}
