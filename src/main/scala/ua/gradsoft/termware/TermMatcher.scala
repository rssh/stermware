package ua.gradsoft.termware;

import TermMatchingKind._;

/**
 * TermMatcher
 **/
trait TermMatcher
{

  def matchingKind: TermMatchingKind;

  /**
   * target pattern name.  
   *   Used only if matching is by name and arity or by-name.
   **/
  def patternName: Name;
 
  /**
   * target arity (defined if possible)
   **/
  def arity: Int;

  /**
   * check, if this matcher match term t with phase 2 (i.e. when name and
   * arity are known to match, we must check typing and so-on).
   **/
  def matchPhase2(t:Term, s: Substitution, vm: VM):(Boolean,Substitution)

  
  def isQuickFalse: Boolean;

}

