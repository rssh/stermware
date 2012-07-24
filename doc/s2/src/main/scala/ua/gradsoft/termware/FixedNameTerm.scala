package ua.gradsoft.termware;

trait FixedNameTerm extends Term
{

  def name = signature.fixedName.get;

}
