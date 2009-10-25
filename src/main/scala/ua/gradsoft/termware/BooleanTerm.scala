package ua.gradsoft.termware;

import ua.gradsoft.termware.vm.VM;


class BooleanTerm(v:Boolean, s: BooleanTermSignature) extends PrimitiveTerm
{

  override def isBoolean: Boolean = true;

  override def getBoolean: Option[Boolean] = Some(v_);

  def termSignature = s_;


  def termUnify(t: Term, s: Substitution)
   = 
     if (t.isBoolean) 
       if (t.getBoolean.get==v_) Some(s)
       else None
     else
       if (t.isX) 
         s+(t.getXIndex.get -> this)
       else
         None
   ;

  lazy val name = s_.theory.symbolTable.getOrCreateElement(
                                        if (v_) "true" else "false" );

  private val v_ = v;
  private val s_ = s;
}

