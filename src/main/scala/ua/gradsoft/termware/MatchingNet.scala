package ua.gradsoft.termware;


import scala.collection.Set;
import scala.collection.mutable.ArrayBuffer;
import scala.collection.immutable.TreeMap;
import ua.gradsoft.termware.flow._;

object MatchingNet
{
  def apply(th:Theory)=new MatchingNet(th);
}

trait MatchingCondition
{
  def check(t:Term,s:Substitution):ComputationBounds[(Boolean,Substitution)]
}

object TrueMatchingCondition extends MatchingCondition
{
  def check(t:Term,s:Substitution):ComputationBounds[(Boolean,Substitution)] = Done((true,s));
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
    
       def check(t:Term,s:Substitution):ComputationBounds[Pair[Boolean,Substitution]]
             =condition.check(t,s);

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


  def doMatch(t:Term)(implicit ctx:CallContext):ComputationBounds[Either[Failure,Success]]=
    doMatchStep(begTerm=t,
                inTerm=t,
                inZi=BigInt(1),
                inSubst=STMSubstitution.empty,
                inUpTerm=None,
                inChildIndex=0,
                inStateIndex=0,
                inNodes=autoStates(0).nc.find(t.name,t.arity),
                inLastNode=None,
                inTrampolinedCheck=None)(ctx);
                 

  def doMatchStep(
                  begTerm: Term,
                  inTerm: Term,
                  inZi: BigInt,
                  inSubst: STMSubstitution,
                  inUpTerm: Option[Term],
                  inChildIndex: Int,
                  inStateIndex: Int,
                  inNodes: List[Node],
                  inLastNode: Option[Node],
                  inTrampolinedCheck: Option[Pair[Boolean,Substitution]]
                   )(implicit ctx:CallContext): ComputationBounds[Either[Failure,Success]] =
  ctx. withCall {
    (ctx:CallContext) => implicit val ictx=ctx;
    var cZi = inZi;
    var cTerm = inTerm;
    var cUpTerm = inUpTerm;
    var cChildIndex = inChildIndex;
    var cStateIndex = inStateIndex;
    var cSubst = inSubst;
    var cNodes = inNodes;
    var cTest = false;
    var cLastNode = inLastNode;
    var optResult: Option[Either[Failure,Success]] = None;
    var trampolinedCheck=inTrampolinedCheck;
    while(!cNodes.isEmpty && optResult!=None) {
      val node = cNodes.head;
      val check = (if (trampolinedCheck==None) {
                       node.check(cTerm,cSubst);
                   } else {
                       val tmp = trampolinedCheck.get;
                       trampolinedCheck=None;
                       Done(tmp);
                   });
      if (!check.isDone) {
        throw new CallCCException(
            CallCC.compose(check, {
                (r:Pair[Boolean,Substitution], ctx:CallContext) => 
                            doMatchStep(begTerm,cTerm,cZi,cSubst,cUpTerm,cChildIndex,
                                        cStateIndex,cNodes,cLastNode,Some(r))(ctx)
              }));
      }else{
        cLastNode = Some(node);
        var (cTest,nextSubst:STMSubstitution) = check.result.get;
        if (cTest) {
          cSubst=nextSubst; 
          node.zipIndexStep match {
            case ZI_DOWN => 
                    if (cTerm.arity > 0) {
                        cUpTerm = Some(cTerm);
                        cTerm=cTerm.subterms(0);
                        cChildIndex=0;
                        cZi=cZi*2;
                    } else {
                        optResult=Some(Left(Failure(begTerm,"down step is not available",None)));
                    }  
            case ZI_RIGHT => 
                    if (cUpTerm==None) {
                        optResult=Some(Left(Failure(begTerm,"right step is not available",None)));
                    } else {
                        cChildIndex=cChildIndex+1; 
                        val up = cUpTerm.get;
                        if (cChildIndex >= up.arity) {
                          optResult=Some(Left(Failure(begTerm,"right step is not available",None)));
                        } else {
                          cTerm = up.subterms(cChildIndex); 
                          cZi=cZi*2+1;
                        }
                    }
            case ZI_FINAL => 
                    optResult=Some(Right(Success(cSubst,node)))
            case _   =>
                     throw new TermWareException("internal error: invalid zipIndexStep"); 
          }
          if (optResult==None) {
            cStateIndex = node.nextAutoState;
            cNodes = autoStates(cStateIndex).nc.find(cTerm.name,cTerm.arity);
          }
        } else {
          cNodes = cNodes.tail;
        }
      }
      if (optResult==None && cNodes.isEmpty) {
        cLastNode match {
         case Some(lastNode) => {
            cZi = lastNode.zipIndexOnFail;
            cStateIndex = lastNode.autoStateOnFail;
            var(cUpTerm,optCurrTerm,cChildIndex)=ZipIndex(begTerm,cZi);
            if (optCurrTerm==None) {
                optResult=Some(Left(Failure(begTerm,"invalid zip index on fail",None)))
            } else {
                cTerm=optCurrTerm.get;
            }
         }
         case None => optResult=Some(Left(Failure(begTerm,"empty node sequence",None)))
        }
      }
      cSubst = cSubst withIndex cZi;
    }
    if (optResult!=None) {
       Done(optResult.get);
    } else {
       Done(Left(Failure(begTerm,"behind constant loop",None)));
    }
  }
             

  class NodeCollect
  {
    var fnfa = Map[Int,Map[Name,Set[Int]]]();
    var fnaa = Map[Name,Set[Int]]();
    var anfa = Map[Int,Set[Int]]();
    var anaa = Set[Int]();
  }

  
  def build(rules:List[TermRule]):Unit={
    val zi = BigInt(1);
    var upR = Map[Int,Term](); 
    var cR = Map[Int,Term](); 
    var allS = Set[Int]();
    var noneS = Set[Int]();
    rules.foreach(
      r=>{
        val nRule = allRules.length;
        allRules.append(r);
        val pattern = r.pattern;
        cR += nRule -> pattern; 
        allS += nRule;
      }
    );
    val allOutState = getOrCreateOutState(allS);
    val noneOutState = getOrCreateOutState(noneS);
    // TODO: add to none fail node.
    build(zi,allOutState,noneOutState,zi,upR,cR,0);
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
