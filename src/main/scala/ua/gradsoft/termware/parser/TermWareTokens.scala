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

  object TokenType extends Enumeration
  {
   val BOOLEAN, STRING, CHAR,
       INT, SHORT, LONG,
       DOUBLE, FLOAT,
       BIG_DECIMAL, BIG_INT,
       IDENTIFIER, KEYWORD,
       DELIMITER, 
       BINARY_OPERATOR,
       ERROR = Value;
  }

  trait TermWareToken extends Token with Positional
  {
   def tokenType: TokenType.Value;
  }

  case class ValueToken[T](t:TokenType.Value, v:T) extends TermWareToken
  {
   val tokenType=t;
   val value=v;
   def chars=value.toString;
  }



  case class TermWareErrorToken(s:String) extends TermWareToken
  {
   def tokenType = TokenType.ERROR;
   val chars=s;
  }

  /**
   * delimiter.
   **/
  case class D(v:String) extends TermWareToken
  {
   def tokenType = TokenType.DELIMITER;
   val chars = v;
  }

  /**
   * binary operator
   *@param v - string representation of operator (i. e. "+")
   *@param p - priority
   *@param fn - appropriative functional symbol (i. e. "plus")
   *@param la - left associativity.
   **/
  case class BinaryOperator(v:String, p:Int, fn:String, la:Boolean) extends TermWareToken
  {
   def tokenType = TokenType.BINARY_OPERATOR;
   def isLeftAssoc = leftAssoc;
   def isRightAssoc = (! leftAssoc);
   val chars = v;
   val priority = p;
   val functionName = fn;
   val leftAssoc = la;
  }

}
