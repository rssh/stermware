package ua.gradsoft.termware;

import scala.collection.Set;
import scala.collection.mutable.ArrayBuffer;
import scala.collection.immutable.TreeMap;

object MatchingNet
{
  def apply(th:Theory)=new MatchingNet(th);
}

trait MatchingCondition
{
  def check(t:Term,s:Substitution,vm:VM):(Boolean,Substitution) 
}

object TrueMatchingCondition extends MatchingCondition
{
  def check(t:Term,s:Substitution,vm:VM):(Boolean,Substitution) = (true,s);
}

trait MatchingNetConstants
{
  val ZI_DOWN = 0;
  val ZI_RIGHT = 1;
  val ZI_FINAL = 2;

  val T = TrueMatchingCondition;
}


class MatchingNet(val theory:Theory)
                         extends MatchingNetConstants
{

  class Node(
    val zi: BigInt,
    val condition:MatchingCondition,
    val zipIndexStep:Int,
    val nextOutState:Int,
    val nextAutoState:Int,
    val stateIndexOnFail: Int,
    val zipIndexOnFail: BigInt,
    val autoStateOnFail: Int
  ) {

       def this(zi:BigInt,
                condition:MatchingCondition,
                zipIndexStep:Int,
                nextOutState:OutState,
                nextFailState:OutState,
                zipIndexOnFail:BigInt) = 
         this(zi,condition,zipIndexStep,
                 nextOutState.index, 
                 getAutoStateIndex(nextOutState,zi),
                 nextFailState.index,
                 zipIndexOnFail,
                 getAutoStateIndex(nextFailState,zi));
    
       def check(t:Term,s:Substitution,vm:VM):(Boolean,Substitution)
             =condition.check(t,s,vm);

       def isFinal:Boolean = (zipIndexStep==ZI_FINAL);
  }


  class NodeChooser(
    var fnfa: Map[Int,Map[Name,List[Node]]],
    var fnaa: Map[Name,List[Node]],
    var anfa: Map[Int,List[Node]],
    var anaa: List[Node],
    var stateIndex: Int = -1
  ) {

      def this() = this(Map[Int,Map[Name,List[Node]]](),
                    Map[Name,List[Node]](),
                    Map[Int,List[Node]](),
                    List[Node]());

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

      def addFNFA(n:Name,a:Int,nd:Node)= {
        var byName = fnfa.getOrElse(a,Map[Name,List[Node]]());
        var l = byName.getOrElse(n,List[Node]());
        l=nd :: l;
        byName += n->l ;
        fnfa += a -> byName;
      }

      def addFNAA(n:Name,nd:Node)= {
        var l = fnaa.getOrElse(n,List[Node]());
        l = nd :: l;
        fnaa += n -> l ;
      }

      def addANFA(a:Int,nd:Node)= {
        var l = anfa.getOrElse(a,List[Node]());
        l = nd :: l;
        anfa += a -> l ;
      }

      def addANAA(nd:Node)= {
        anaa = nd::anaa ;
      }


  }

  class OutState(
    val index: Int,
    val ruleIndexes: Set[Int]
  )
  {
    def getNodes(zi:BigInt,n:Name,a:Int):List[Node]={
       choosers.get(zi) match {
         case None => List.empty;
         case Some(x)  => x.find(n,a);
       }
    }

    def rules:List[TermRule] = {
       for(i <- ruleIndexes.toList) yield allRules(i);
    }
  
    def addFNFA(zi:BigInt,name:Name,arity:Int,node:Node) = {
       val nc = getNodeChooser(zi);
       nc.addFNFA(name,arity,node);
    }

    def addFNAA(zi:BigInt,name:Name,node:Node) = {
      val nc = getNodeChooser(zi);
      nc.addFNAA(name,node);
    }

    def addANFA(zi:BigInt,arity:Int,node:Node) = {
      val nc = getNodeChooser(zi);
      nc.addANFA(arity,node);
    }

    def addANAA(zi:BigInt,node:Node) = {
      val nc = getNodeChooser(zi);
      nc.addANAA(node);
    }

    def getNodeChooser(zi:BigInt):NodeChooser = {
      val optNc = choosers.get(zi);
      val retval = if (optNc==None) {
                       val nc = new NodeChooser();
                       choosers += zi -> nc;
                       nc;
                   } else {
                       optNc.get;
                   }
      return retval;
    }

    def withCondition:Boolean = {
      ruleIndexes.find(allRules(_).withCondition) != None ;
    }

    var choosers = TreeMap[BigInt,NodeChooser](); 
  }

  case class AutoState(val outState:OutState, 
                       val zi:BigInt, 
                       val nc: NodeChooser);

  def getAutoStateIndex(outState:OutState, zi:BigInt):Int = {
    val nc = outState.getNodeChooser(zi);
    if (nc.stateIndex == -1) {
       val state = new AutoState(outState,zi,nc);
       nc.stateIndex=autoStates.length;
       autoStates.append(state); 
    }
    nc.stateIndex;
  }


  val outStates = new ArrayBuffer[OutState];
  val autoStates = new ArrayBuffer[AutoState];
  val allRules = new ArrayBuffer[TermRule];

  case class Failure(val t:Term, val m:String, val prev:Option[Failure])
  case class Success(val s:STMSubstitution, val node:Node)


  def doMatch(t:Term,vm:VM):Either[Failure,Success]={
    var czi=BigInt(1);
    var csi=0;
    var ct=t;
    var ctup:Option[Term]=None;
    var cti=0;
    var quit:Boolean=false;
    var cs = STMSubstitution.empty;
    //var outState=outStates(csi);
    var autoState=autoStates(csi);
    while(!quit) {
       //var nodes=outState.getNodes(czi,ct.name,ct.arity); 
       var nodes=autoState.nc.find(ct.name,ct.arity); 
       var test = false;
       var lastNode:Option[Node] = None;
       while(!nodes.isEmpty && !test) {
         var node = nodes.head;
         lastNode = Some(node);
         nodes=nodes.tail;
         var (test,s:STMSubstitution) = node.check(ct,cs,vm);
         if (test) {
           //csi=node.nextOutState;
           csi=node.nextAutoState;
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
                                 return Right(Success(cs,node));
                               }
              case _ => { throw new TermWareException(
                                 "internal error: invalid zipIndexStep"); 
                        }
           }
        }
       }
       if (!test) {
        if (lastNode!=None) {
         var node = lastNode.get;
         //csi=node.stateIndexOnFail;
         czi=node.zipIndexOnFail;
         csi=node.autoStateOnFail;
         var(ctup,oct,cti)=ZipIndex(t,czi);
         if (oct==None) {
           return Left(Failure(t,"invalid zipIndex on fail",None));
         }else{
           ct=oct.get;
         }
        } else {
         // lastNode = None.
         return Left(Failure(t,"empty node sequence",None));
        }
       }
       cs = cs withIndex czi;
       //outState=outStates(csi);
       autoState = autoStates(csi);
    }
    return Left(Failure(t,"Impossible: behind constant loop",None));
  }

  class NodeCollect
  {
    var fnfa = Map[Int,Map[Name,Set[Int]]]();
    var fnaa = Map[Name,Set[Int]]();
    var anfa = Map[Int,Set[Int]]();
    var anaa = Set[Int]();
  }

  def build(zi: BigInt, 
            prevOutState: OutState,
            failOutState: OutState, 
            failZi      : BigInt,
            upRuleParts: Map[Int,Term], 
            cRuleParts: Map[Int,Term], 
            hi: Int
            ):Unit = {
    val nc = createNodeCollect(zi,cRuleParts); 
    for((arity,byName)<-nc.fnfa) {
     for((name,ruleIndexes) <- byName) {
       val nextOwnState = getOrCreateOutState(ruleIndexes);
       var (nextFailState:OutState,nextFailZi:BigInt) = buildFailState(zi,
                                          prevOutState,nextOwnState,
                                          failOutState, failZi,
                                          upRuleParts, cRuleParts, hi);
       if (arity == 0) {
         val n = new Node(zi,T,ZI_FINAL,nextOwnState,nextFailState,nextFailZi);
         prevOutState.addFNFA(zi,name,arity,n);
       }else{
         var n = new Node(zi,T,ZI_DOWN,nextOwnState,nextFailState,nextFailZi);
         prevOutState.addFNFA(zi,name,arity,n);
         var curOwnState = nextOwnState;
         var rZi=zi*2;
         val rUp=cRuleParts.filter(a=>ruleIndexes(a._1));
         var i=0;
         var endLoop=false;
         while(i<arity && !endLoop) {
            val rightRules:Map[Int,Term] = collectChilds(rUp,i);
            if (!rightRules.isEmpty) {
              val rightIndexes = rightRules.keySet;
              val rightState = getOrCreateOutState(rightIndexes);
              val ziDirection = if (i==arity-1)  ZI_FINAL else ZI_RIGHT;
              rZi=rZi*2+1;
              var (nextFailState1:OutState, nextFailZi1:BigInt) = 
                                        buildFailState(rZi,
                                           curOwnState, rightState,
                                           nextFailState, nextFailZi,
                                           rUp, rightRules, i); 
              nextFailState=nextFailState1;
              nextFailZi=nextFailZi1;
              var node = new Node(rZi,T,ziDirection,rightState, 
                                           nextFailState, nextFailZi);
              prevOutState.addFNFA(rZi,name,i,node);
              build(rZi,nextOwnState,nextFailState,nextFailZi, 
                                           rUp,rightRules,i);
            }else{
              endLoop=true;
            }
            i=i+1;
         }
       }
     }
    }
    for( (name,ruleIndexes) <- nc.fnaa) {
       var nextOutState = getOrCreateOutState(ruleIndexes);
       var (nextFailState:OutState, nextFailZi:BigInt) = buildFailState(zi,
                                          prevOutState,nextOutState,
                                          failOutState,failZi,
                                          upRuleParts, cRuleParts, hi);
       val n = new Node(zi,T,ZI_FINAL,nextOutState,nextFailState,nextFailZi);
       prevOutState.addFNAA(zi,name,n);
    }
    for( (arity,ruleIndexes) <- nc.anfa) {
       var nextOutState = getOrCreateOutState(ruleIndexes);
       var (nextFailState:OutState, nextFailZi:BigInt) = buildFailState(zi,
                                          prevOutState,nextOutState,
                                          failOutState,failZi,
                                          upRuleParts, cRuleParts, hi);
       val n = new Node(zi,T,ZI_FINAL,nextOutState,nextFailState,nextFailZi);
       prevOutState.addANFA(zi,arity,n);
    }
    if (!nc.anaa.isEmpty) {
       var nextOutState = getOrCreateOutState(nc.anaa);
       var (nextFailState:OutState, nextFailZi:BigInt) = buildFailState(zi,
                                          prevOutState,nextOutState,
                                          failOutState,failZi,
                                          upRuleParts, cRuleParts, hi);
       val n = new Node(zi,T,ZI_FINAL,nextOutState,nextFailState,nextFailZi);
       prevOutState.addANAA(zi,n);
    }
   }


   def buildFailState(zi:BigInt,
            prevState:OutState,nextState:OutState,
            prevFailState:OutState,prevFailZi:BigInt,
            upRuleParts: Map[Int,Term], 
            cRuleParts: Map[Int,Term], 
            hi:Int
            ):(OutState,BigInt) = {
      val newFailRuleIndexes = nextState.ruleIndexes -- nextState.ruleIndexes;
      if (newFailRuleIndexes.isEmpty || !nextState.withCondition) {
         (prevFailState,prevFailZi);
      } else {
         val newFailState = getOrCreateOutState(newFailRuleIndexes);
         build(zi,newFailState, prevFailState, prevFailZi, 
               upRuleParts, cRuleParts, hi);
         (newFailState,zi);
      }
   }
   
  
   def createNodeCollect(zi:BigInt,cRuleParts:Map[Int,Term])
                                                :NodeCollect = {
     val retval = new NodeCollect();
     for( (ruleIndex, term) <- cRuleParts ) {
       // term is exactly ziIndex.
       if (term.patternName != None) {
        val patternName = term.patternName.get;
        if (term.patternArity != None) {
          val patternArity = term.patternArity.get;
          var byName = retval.fnfa.getOrElse(patternArity,
                                             TreeMap[Name,Set[Int]]());
          val s = byName.getOrElse(patternName,Set())+ruleIndex;
          byName += patternName -> s ;
          retval.fnfa += patternArity -> byName;
        } else {
          // term.patternArity = None
          var byName = retval.fnaa;
          var s=byName.getOrElse(patternName,Set())+ruleIndex;
          retval.fnaa += patternName -> s;
        }
       } else {
        // term.patternName = None
        if (term.patternArity != None) {
          val patternArity = term.patternArity.get;
          val s = retval.anfa.getOrElse(patternArity,Set[Int]())+ruleIndex;
          retval.anfa += patternArity -> s;
        } else {
          retval.anaa += ruleIndex;
        }
       }
     }
     
     return retval;  
   }
     
   /**
    * collect i=th childs of appropriative rule patterns
    **/
   def collectChilds(rUp:Map[Int,Term],i:Int):Map[Int,Term] = {
      var retval=Map[Int,Term]();
      for((ri,t)<-rUp) {
        if (i<t.arity) {
          retval += ri->t.subterm(i);
        }
      }
      return retval;
   }

   def getOrCreateOutState(ri:Set[Int]):OutState = 
   {
     var optRetval = outStates.find(_.ruleIndexes==ri);
     if (optRetval != None) {
       optRetval.get;
     } else {
       var retval = new OutState(outStates.length,ri);
       outStates.append(retval);
       return retval;
     } 
   }

}
