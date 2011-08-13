package ua.gradsoft.termware;

import java.io.PrintWriter;
import ua.gradsoft.termware.flow._;

case class TermBinding(val name: Name, val kind:Int, var value: ComputationBounds[Term])
{

  def subst(s:PartialFunction[Term,Term])(implicit ctx:CallContext)
                                                         :TermBinding =
  {
   kind match { 
    case TermBinding.EAGER => TermBinding(name,kind, 
                                 CallCC.compose(value,
                                  {(t:Term,ctx:CallContext) => t.subst(s)(ctx)}
                              ))
    case TermBinding.LAZY => 
           val newBind = new TermBinding(name,kind,null);
           val newValue = Call{ (ctx:CallContext) =>
                            newBind.value =  CallCC.compose(value,
                                {(t:Term,ctx:CallContext) => t.subst(s)(ctx); }
                              )(ctx);
                            newBind.value;
                            };
           newBind.value = newValue;
           newBind;
   }
  }

  def print(out:PrintWriter):Unit = {
     out.print(name);
     out.print(" ");
     out.print(arrow);
     out.print(" ");
     if (value.isDone) {
          value.result.get.print(out);
     } else {
          out.print("..computation..");
     }
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
