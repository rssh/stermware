package ua.gradsoft.termware;

import scala.collection.mutable.ArrayBuffer;

object MatchingNet
{
  def apply(th:Theory)=new MatchingNet(th);
}

trait MatchingCondition
{
  def check(t:Term,s:Substitution,vm:VM):(Boolean,Substitution) 
}

trait MatchingNetConstants
{
  val ZI_DOWN = 0;
  val ZI_RIGHT = 1;
  val ZI_FINAL = 2;

}

class MatchingNet(val theory:Theory)
                         extends MatchingNetConstants
{

  class Node(
    val condition:MatchingCondition,
    val nextState:Int,
    val zipIndexStep:Int
  ) {
       def check(t:Term,s:Substitution,vm:VM):(Boolean,Substitution)
             =condition.check(t,s,vm);

       def isFinal:Boolean = (zipIndexStep==ZI_FINAL);
  }

  class NodeChooser(
    val fnfa: Map[Int,Map[Name,List[Node]]],
    val fnaa: Map[Name,List[Node]],
    val anfa: Map[Int,List[Node]],
    val anaa: List[Node]
  ) {

      def find(n:Name,a:Int):List[Node]={
         var r=findFNFA(n,a);
         if (r!=None) return r.get;
         r=fnaa.get(n);
         if (r!=None) return r.get;
         r=anfa.get(a);
         if (r!=None) return r.get;
         return anaa;
      }

      def findFNFA(n:Name,a:Int)={
        fnfa.get(a) match {
           case None => None
           case Some(x) => x.get(n)
        }
      }

  }

  class State(
    val index: Int,
    val choosers: Map[BigInt,NodeChooser],
    val rules: List[TermRule],
    val stateIndexOnFail: Int,
    val zipIndexOnFail: BigInt
  )
  {

    def getNodes(zi:BigInt,n:Name,a:Int):List[Node]={
       choosers.get(zi) match {
         case None => List.empty;
         case Some(x)  => x.find(n,a);
       }
    }
  
  }



  val states = new ArrayBuffer[State];

  case class Failure(val t:Term, val m:String, val prev:Option[Failure])
  case class Success(val s:STMSubstitution, val rules:List[TermRule])


  def doMatch(t:Term,vm:VM):Either[Failure,Success]={
    var czi=BigInt(1);
    var csi=0;
    var ct=t;
    var ctup:Option[Term]=None;
    var cti=0;
    var quit:Boolean=false;
    var cs = STMSubstitution.empty;
    var state=states(csi);
    while(!quit) {
       var nodes=state.getNodes(czi,ct.name,ct.arity); 
       var test = false;
       while(!nodes.isEmpty && !test) {
         var node = nodes.head;
         nodes=nodes.tail;
         var (test,s:STMSubstitution) = node.check(ct,cs,vm);
         if (test) {
           csi=node.nextState;
           cs=s;
           node.zipIndexStep match {
              case ZI_DOWN => {
                               if (ct.arity > 0) {
                                 ct=ct.subterms(0);
                                 ctup=Some(ct);
                                 cti=0;
                                 czi=czi*2;
                               } else {
                                 // impossible
                                 return Left(
                                     Failure(t,"down is not avalilable",None));
                               }
                              }
              case ZI_RIGHT => {
                              if (ctup==None) {
                                 // impossible
                                 return Left(
                                   Failure(t,"right is not available",None));
                              } else {
                                 val up=ctup.get;
                                 cti=cti+1;
                                 if (cti < ct.arity) {
                                   ct=up.subterms(cti);
                                 }else{
                                   // impossible
                                   return Left(
                                     Failure(t,"right is not available",None));
                                 }
                                 czi=czi*2+1;
                              }
                             }
              case ZI_FINAL => {
                                 return Right(Success(cs,state.rules));
                               }
              case _ => { throw new TermWareException(
                                 "internal error: invalid zipIndexStep"); 
                        }
           }
        }
       }
       if (!test) {
         csi=state.stateIndexOnFail;
         czi=state.zipIndexOnFail;
         var(ctup,oct,cti)=ZipIndex(t,czi);
         if (oct==None) {
           return Left(Failure(t,"invalid zipIndex on fail",None));
         }else{
           ct=oct.get;
         }
       }
       cs = cs withIndex czi;
       state=states(csi);
    }
    return Right(Success(cs,state.rules));
  }


}
