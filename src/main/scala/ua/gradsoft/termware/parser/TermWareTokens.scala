package ua.gradsoft.termware.parser;

import scala.util.parsing.syntax._;
import scala.util.parsing.combinator._;
import scala.util.parsing.combinator.lexical._;
import scala.util.parsing.input._;

import ua.gradsoft.termware._;

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
       OPERATOR,
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
  case class OperatorToken(op2:BinaryOperator,op1:UnaryOperator) 
                                              extends TermWareToken
  {
   def tokenType = TokenType.OPERATOR;
   def chars = if (v2!=null) v2.sign else v1.sign;
   val v2 = op2;
   val v1 = op1;
  }

  case class KeywordToken(v:String) extends TermWareToken
  {
   def tokenType = TokenType.KEYWORD;
   val chars = v;
  }

}
