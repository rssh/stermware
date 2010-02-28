package ua.gradsoft.termware;

trait SimpleUnifyWithVM extends SimpleUnify
{

  override def termUnify(t:Term, s:Substitution) 
    =  termUnify(t,s,new VM);
    
}
