package ua.gradsoft.termware.parser;

import scala.util.parsing.syntax._;
import scala.util.parsing.combinator._;
import scala.util.parsing.combinator.lexical._;
import scala.util.parsing.input._;

/**
 * tokens for termware language
 **/
trait TermWareTokens extends Tokens
{

  trait TermWareToken extends Token with Positional
  {}

  /**
   * boolean primitive
   **/
  case class BooleanToken(v:Boolean) extends TermWareToken
  {
   val value = v;
   def chars = v.toString;
  }

  /**
   * delimiter.
   **/
  case class D(v:String) extends TermWareToken
  {
   val chars = v;
  }

  /**
   * operator
   **/
  case class Op(v:String) extends TermWareToken
  {
   val chars = v;
  }

}
