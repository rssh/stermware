package ua.gradsoft.termware;

trait TermOps
{
  this:Term =>

  def + (t:Term) = signature.theory.createFunTerm("plus",this,t);
  def unary_+  = signature.theory.createFunTerm("unary_plus",this);
  def - (t:Term) = signature.theory.createFunTerm("minus",this);
  def unary_-  = signature.theory.createFunTerm("unary_minus",this);

  def * (t:Term) = signature.theory.createFunTerm("multiply",this,t);
  def / (t:Term) = signature.theory.createFunTerm("divide",this,t);

  def % (t:Term) = signature.theory.createFunTerm("percent",this,t);

 //
 //this is reserved by Ordered
 // def < (t:Term) = signature.theory.createFunTerm("less",this,t);
 // def <= (t:Term) = signature.theory.createFunTerm("lessEq",this,t);
 // def > (t:Term) = signature.theory.createFunTerm("greater",this,t);
 // def >= (t:Term) = signature.theory.createFunTerm("greaterEq",this,t);

  // eq and new does not overloaded.
  //def == (t:Term) = signature.theory.createFunTerm("greaterEq",this,t);

  def && (t:Term) = signature.theory.createFunTerm("and",this,t);
  def and (t:Term) = signature.theory.createFunTerm("and",this,t);
  def || (t:Term) = signature.theory.createFunTerm("or",this,t);
  def or (t:Term) = signature.theory.createFunTerm("or",this,t);
  def unary_! = signature.theory.createFunTerm("not",this);

  def & (t:Term) = signature.theory.createFunTerm("bitand",this,t);
  def | (t:Term) = signature.theory.createFunTerm("bitor",this,t);
  def ^ (t:Term) = signature.theory.createFunTerm("bitxor",this,t);
  def unary_~  = signature.theory.createFunTerm("bitnot",this);

  def << (t:Term) = signature.theory.createFunTerm("leftShift",this,t);
  def >> (t:Term) = signature.theory.createFunTerm("rightShift",this,t);


}
