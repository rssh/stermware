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
       DELIMITER, OPERATOR, ERROR = Value;
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


  case class CharToken(ch:Char) extends TermWareToken
  {
   def tokenType = TokenType.CHAR;
   val value = ch;
   def chars = value.toString;
  }


  case class IntToken(v:Int) extends TermWareToken
  {
   def tokenType = TokenType.INT;
   val value = v;
   def chars = value.toString;
  }

  case class LongToken(v:Long) extends TermWareToken
  {
   def tokenType = TokenType.LONG;
   val value = v;
   def chars = value.toString;
  }

  case class ShortToken(v:Short) extends TermWareToken
  {
   def tokenType = TokenType.SHORT;
   val value = v;
   def chars = value.toString;
  }

  case class BigIntToken(v:BigInt) extends TermWareToken
  {
   def tokenType = TokenType.BIG_INT;
   val value = v;
   def chars = value.toString;
  }

  case class BigDecimalToken(v:BigDecimal) extends TermWareToken
  {
   def tokenType = TokenType.BIG_DECIMAL;
   val value = v;
   def chars = value.toString;
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
   * operator
   **/
  case class Op(v:String) extends TermWareToken
  {
   def tokenType = TokenType.OPERATOR;
   val chars = v;
  }

}
