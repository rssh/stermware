package ua.gradsoft.termware;


trait BaseTermRuleSet 
{
 
  /**
   * get candidates for rules, matching y term, ordered in 
   * 'more concrete - first'.
   **/
   def candidates(t:Term): Iterator[TermRule]
   

}
