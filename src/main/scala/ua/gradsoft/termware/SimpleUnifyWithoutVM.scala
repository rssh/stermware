package ua.gradsoft.termware;

trait SimpleUnifyWithoutVM extends SimpleUnify
{

  override def termUnify(t:Term, s:Substitution, vm:VM) 
    =  termUnify(t,s);
    
}
