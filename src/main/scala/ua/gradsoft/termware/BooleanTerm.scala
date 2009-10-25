package ua.gradsoft.termware;

import ua.gradsoft.termware.vm.VM;

class BooleanTerm(v:Boolean) extends PrimitiveTerm
{

  override def isBoolean: Boolean = true;

  override def getBoolean: Option[Boolean] = Some(v_);

  def termUnify(t: Term, s: Substitution)
   = 
     if (t.isBoolean) 
       if (t.getBoolean==v_) Some(s)
       else None
     else
       if (t.isX) 
         s+(t.getXIndex.get -> this)
       else
         None
   ;

  val v_ = v;
}


