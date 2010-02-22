package ua.gradsoft.termware.fn;

import ua.gradsoft.termware.VM;

object FnSeqReverse
{
  def mk(commands: (VM=>VM)*):(VM=>VM) = new FnSeqReverse(commands:_*);
}

/**
 * put commands in reverse mode
 **/
case class FnSeqReverse(commands: (VM=>VM)*) extends Function1[VM,VM]
{

  def apply(vm:VM):VM = { vm.pushCommands(seq:_*); return vm; }

  val seq:Seq[VM=>VM] = commands.reverse;
}
