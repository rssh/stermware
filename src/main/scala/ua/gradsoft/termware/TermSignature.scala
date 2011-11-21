package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

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
   *  allow one. Otherwise - throw UnsupportedOperationException.
   */
   def createTerm(name:Name, args: IndexedSeq[Term]): Term;

   def fixCreate(name:Name, args:IndexedSeq[Term]): Term 
         = createTerm(name, args);

   def createTerm(name:Name, args: Term*): Term = {
          createTerm(name,args.toIndexedSeq)
   }

  /**
   * create term in computation bounds.
   */
   def createTerm(name:Name, args:IndexedSeq[ComputationBounds[Term]])
                 (implicit ctx:CallContext):ComputationBounds[Term] = 
     CallCC.compose(CallCC.seq(args),
                    { 
                      l:IndexedSeq[Term] => Done(fixCreate(name,l));
                    });
   
   def createTerm(name:Name, args: ComputationBounds[Term]*)
                 (implicit ctx:CallContext):ComputationBounds[Term] = 
   {
    args match {
     case x: IndexedSeq[ComputationBounds[Term]] => createTerm(name,x)
     case _ => {
       val arr = new Array[ComputationBounds[Term]](args.length);
       for( i <- 0 to args.length-1) {
          arr.update(i,args(i));
       }
       createTerm(name,arr);
     }
    }
   }

   def createTerm(name:String, args: IndexedSeq[Term]):Term =
    createTerm(theory.symbolTable.getOrCreate(name),args);

   def createTerm(name:String, args: Term*):Term =
    createTerm(theory.symbolTable.getOrCreate(name),args:_*);

   def createTerm(name:String, args: ComputationBounds[Term]*)
                 (implicit ctx:CallContext):ComputationBounds[Term] = 
    createTerm(theory.symbolTable.getOrCreate(name),args:_*)(ctx);

   def createTerm(name:String, args: IndexedSeq[ComputationBounds[Term]])
                 (implicit ctx:CallContext):ComputationBounds[Term] = 
    createTerm(theory.symbolTable.getOrCreate(name),args);

    
   /**
    * get constant, defined by object
    */
   def createConstant(arg:Any): Term;
    
   /**
    * create special construct (names are difined in subclasses).
    */
   def createSpecial(args: Any*): Term;

   /**
    * calcultate type of term 
    **/
   def termType(t:Term): Term;

   /**
    * get theory, where signature was defined
    **/
   def theory: Theory;

   /**
    * transform term to native language object reference.
    **/
   def toAnyRef(t:Term):AnyRef;

   /**
    * transform term to native language object.
    **/
   def toAny(t:Term):Any;

   /**
    * transform native language object reference to term.
    **/
   def fromAnyRef(x:AnyRef): Option[Term];

   /**
    * transform native language object to term.
    **/
   def fromAny(x:Any):Option[Term];


}
