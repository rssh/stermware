package ua.gradsoft.termware;

/**
 * Term signature. Can be interpreted as behaviourial definition
 * for family of some kinds of term.
 */
trait TermSignature
{

  /**
   * @return fixed arity (or exception if getFixedArity is false.)
   */
  def fixedArity: Option[Int];

  /**
   * @param name = name of subterm.
   * @return index of subterm with name, or null if isNamedSubterm is false
   *  or name is not found.
   */
  def indexByName: Option[Name => Option[Int]];

  /**
   * return index-name of subterm.
   * @param index
   * @return
   */
  def nameByIndex: Option[Int => Option[Name]];

  /**     
   * @return true, if this term can't contains free variables.
   */
  def isConcrete: Boolean;


  /**
   * @return fixed name of objects with such signature or none if one 
   * does not exists
   */
  def  fixedName: Option[Name];


  /**
   * @param name - name of term to create. if isFixedName is true, than
   *  must be same, as getFixedName
   * @param args - arguments to create.
   * @return newly-created term if signature restrictions
   *  allow one. Otherwise - none.
   */
   def createTerm(name:Name, args: Term*): Option[Term];

  /**
   * create term, getting term values from data stack.
   */
   def createTermFn(name:Name, arity:Int): VM=>VM = {
    vm:VM => {
     val args = new Array[Term](arity);
     for( i <- 0 to arity ) {
       args(arity-i-1)=vm.popData.asInstanceOf[Term];
     };
     createTerm(name, args:_*);
     vm;
    }
   }

   def createTerm(name:String, args: Term*):Option[Term] =
    createTerm(theory.symbolTable.getOrCreateElement(name),args:_*);

   def createTermFn(name:String, arity: Int):VM=>VM =
    createTermFn(theory.symbolTable.getOrCreateElement(name),arity);

    
   /**
    * get constant, defined by object
    */
   def createConstant(arg:Any): Option[Term];
    
   /**
    * create special construct (names are difined in subclasses).
    */
   def createSpecial(args: Any*): Option[Term];

   /**
    * calculate type of term
    **/
   def getType(t:Term):Term;

   /**
    * function, which get <code> t </code> from data stack and
    * put calculated type instead.
    **/
   def getTypeFn:VM=>VM;

   /**
    * get theory, where signature was defined
    **/
   def theory: Theory;

}
