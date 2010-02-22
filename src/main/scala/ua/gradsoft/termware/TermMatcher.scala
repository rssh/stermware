package ua.gradsoft.termware;

import TermMatchingKind._;

/**
 * TermMatcher
 *   usd
 **/
trait TermMatcher
{

  def matchingKind: TermMatchingKind;

  /**
   * quick check: is exists sence start unfication process 
   * with term <code> t </code>
   **/
  def preMatch(t:Term): Boolean

}

