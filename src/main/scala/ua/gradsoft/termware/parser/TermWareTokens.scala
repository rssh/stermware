package ua.gradsoft.termware.parser;

import scala.util.parsing.syntax._;
import scala.util.parsing.combinator._;
import scala.util.parsing.combinator.lexical._;

/**
 * tokens for termware language
 **/
trait TermWareTokens extends Tokens
{

  /**
   * boolean primitive
   **/
  case class BooleanToken(v:Boolean) extends Token
  {
   val value = v;
   def chars = v.toString;
  }

  /**
   * delimiter.
   **/
  case class D(v:String) extends Token
  {
   val chars = v;
  }

  /**
   * operator
   **/
  case class Op(v:String) extends Token
  {
   val chars = v;
  }

}
