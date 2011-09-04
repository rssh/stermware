package ua.gradsoft.termware;

import ua.gradsoft.termware.flow._;

class ListTermSignature(th:Theory) 
                                    extends FunctionalTermSignature
{

 def fixedArity:Option[Int] = Some(2);

 def indexByName:Option[Name=>Option[Int]] = Some(
   (n: Name) => if (n==CAR) Some(0)
              else if (n==CDR) Some(1)
              else None
 );

 def nameByIndex:Option[Int=>Option[Name]] = Some(
   (i: Int) => i match {
              case 0 => Some(CAR)
              case 1 => Some(CDR)
              case _ => None;
             }
 )

 def fixedName:Option[Name] = Some(CONS);
 
 def createTerm(name:Name, args:IndexedSeq[Term]) : Term =  
  args.length match {
    case 0 => theory.nilSignature.createConstant(null)
    case 1 => new ListTerm(args(0),
                           theory.nilSignature.createConstant(null),
                           this)
    case 2 => {
                if (isList(args(1))) {
                  new ListTerm(args(0),args(1),this)
                } else {
                  new ListTerm(args(0), 
                           this.createTerm(name, args.drop(1)),this)
                }
              }
    case _ => new ListTerm(args(0), 
                           this.createTerm(name, args.drop(1)),this)
  };
 
 def termType(ct:ComputationBounds[Term])(implicit ctx:CallContext)
                                                 :ComputationBounds[Term] = {
  if (ct.isDone) {
   val t = ct.result.get;
   t.getAttribute(theory.symbolTable.TYPE) match {
      case Some(x) =>  Done(x)
      case None   => {
        val typeIn = theory.freeFunSignature.createTerm(
                               t.name,
                               t.subterms.map( _.termType )
                     );
        val typeOut = theory.typeAlgebra.reduce(typeIn);
        t.setAttribute(theory.symbolTable.TYPE, new ComputationBoundsTerm(typeOut));
        typeOut
      }
   }
  } else {
   CallCC.compose(ct, { (t:Term,ctx:CallContext) => termType(Done(t))(ctx); });
  }
 } 
 
 
 private def isList(t:Term) = t.isNil || (t.name==CONS && t.arity==2);

 val theory = th;

 lazy val CONS = theory.symbolTable.getOrCreate("cons");
 lazy val CAR = theory.symbolTable.getOrCreate("car");
 lazy val CDR = theory.symbolTable.getOrCreate("cdr");

}
