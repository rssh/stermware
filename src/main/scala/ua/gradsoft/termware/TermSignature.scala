package ua.gradsoft.termware;

/**
 * Term signature. Can be interpreted as behaviourial definition
 * for family of some kinds of term.
 */
trait TermSignature
{

  /**
   * @return fixed arity (or None if getFixedArity)
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
   * @return name of subterm, if names by indexes are defined. 
   */
  def nameByIndex: Option[Int => Option[Name]];

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
   def createTerm(name:Name, args: RandomAccessSeq[Term]): Option[Term];

   def createTerm(name:Name, args: Term*): Option[Term] = {
    args match {
     case x: RandomAccessSeq[Term] => createTerm(name,x)
     case _ => {
       val arr = new Array[Term](args.length);
       for( i <- 0 to args.length-1) {
          arr.update(i,args(i));
       }
       createTerm(name,arr);
     }
    }
   }

  /**
   * create term, getting term values from data stack.
   */
   def createTermFn(name:Name, arity:Int): VM=>VM = {
    vm:VM => {
     val args = new Array[Term](arity);
     for( i <- 0 to arity ) {
       args(arity-i-1)=vm.popData.asInstanceOf[Term];
     };
     createTerm(name, args);
     vm;
    }
   }

   def createTerm(name:String, args: RandomAccessSeq[Term]):Option[Term] =
    createTerm(theory.symbolTable.getOrCreate(name),args);

   def createTerm(name:String, args: Term*):Option[Term] =
    createTerm(theory.symbolTable.getOrCreate(name),args:_*);

   def createTermFn(name:String, arity: Int):VM=>VM =
    createTermFn(theory.symbolTable.getOrCreate(name),arity);

    
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
   def getTypeFn:VM=>VM
     = (vm:VM) => { vm.popData match {
                      case t:Term => vm.pushData(getType(t));
                      case _      => throw new ErrorTermException("term on stack expected");
                    }; vm; }

   /**
    * get theory, where signature was defined
    **/
   def theory: Theory;

}
