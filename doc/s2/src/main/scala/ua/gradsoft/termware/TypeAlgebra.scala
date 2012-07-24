package ua.gradsoft.termware;

trait TypeAlgebra extends TermSystem
{

 def top: Term;
 def bottom: Term;

}

object DefaultTypeAlgebraFactsDatabase extends FactsDatabase
{

 def resolveSignal(th:Theory, n:Name,a:Int):
          Option[(Seq[Term],STMSubstitution[Term])=>(Boolean,STMSubstitution[Term])] =
 {
  signals.get(n);
 }

 def resolveAction(th:Theory, n:Name,a:Int): Option[(Seq[Term],STMSubstitution[Term])=>Unit] =
 {
  actions.get(n);
 }

 def resolveFunction(th:Theory, n:Name, a:Int): Option[(Seq[Term],STMSubstitution[Term])=>Term] =
 {
  functions.get(n)
 }


  lazy val signals = Map[Name,(Seq[Term],STMSubstitution[Term])=>(Boolean, STMSubstitution[Term])]();
  lazy val actions = Map[Name,(Seq[Term],STMSubstitution[Term])=>Unit]();
  lazy val functions = Map[Name,(Seq[Term],STMSubstitution[Term])=>Term]();

}

class DefaultTypeAlgebra(fa: FreeAlgebra) 
                           extends StandardTermSystem(
                                   theory = fa,
                                   matching = new MatchingNet(fa),
                                   facts = DefaultTypeAlgebraFactsDatabase,
                                   strategy = FirstLeft
                           )
                           with TypeAlgebra
{


  lazy val top = fa.createAtom("TOP");        // TODO: think about better names
  lazy val bottom = fa.createAtom("BOTTOM");  

}
