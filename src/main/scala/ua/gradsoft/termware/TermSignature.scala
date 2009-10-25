package ua.gradsoft.termware;


trait TermSignature
{

  /**
   * is arity of this term is fixed ?
   * @return if signature of this term fix arity.
   */
  def isFixedArity: Boolean;

  /**
   * @return fixed arity (or exception if getFixedArity is false.)
   */
  def getFixedArity: Option[Int];

  /**    
   * @return true if we have named subterms.
   */
  def isNamedSubterms: Boolean;

  /**
   * @param name = name of subterm.
   * @return index of subterm with name, or null if isNamedSubterm is false
   *  or name is not found.
   */
  def getIndexByName(name:Name): Option[Int];

  /**
   * return index-name of subterm.
   * @param index
   * @return
   */
  def getNameByIndex(index:Int): Option[Name];

  /**     
   * @return true, if this term can't contains free variables.
   */
  def isConcrete: Boolean;


  /**
   * @return true, if signature have fixed name.
   */
  def  isFixedName: Boolean


  /**
   * @return get fixed name of object, or null if one does not exists
   */
  def  getFixedName: Option[Name];

  /**
   *
   * @param name - name of term to create. if isFixedName is true, than
   *  must be same, as getFixedName
   * @param args - arguments to create.
   * @return newly-created term.  If creation is impossible - throw exceptrim
   */
  // def createTerm(Name name, Term ... args): Either[Term];
    
   /**
    * get constant, defined by object.
    */
   def createConstant(arg:Any): Option[Term];
    
   ///**
   // * create special construct (names are difined in subclasses).
   // */
   //def createSpecial(Any ... args): Either[TermRestrictionException,Term];


}
