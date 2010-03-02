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

  case class ValueToken[T](v:T) extends TermWareToken
  {
   val value=v;
   def chars=value.toString;
  }

  /**
   * boolean primitive
   **/
  case class BooleanToken(v:Boolean) extends TermWareToken
  {
   val value = v;
   def chars = value.toString;
   //override def setPos(newPos:Position)  =
   //{ System.err.println("setPos:"+newPos.toString+", class="+newPos.getClass);
   //   super.setPos(newPos);
   //   this;
   // }
  }

  case class StringToken(s:String) extends TermWareToken
  {
   val value = s;
   def chars = value;
  }

  case class CharToken(ch:Char) extends TermWareToken
  {
   val value = ch;
   def chars = value.toString;
  }

  abstract class NumberToken extends TermWareToken 
  {
  };

  case class IntToken(v:Int) extends NumberToken
  {
   val value = v;
   def chars = value.toString;
  }

  case class LongToken(v:Long) extends NumberToken
  {
   val value = v;
   def chars = value.toString;
  }

  case class ShortToken(v:Short) extends NumberToken
  {
   val value = v;
   def chars = value.toString;
  }

  case class BigIntToken(v:BigInt) extends NumberToken
  {
   val value = v;
   def chars = value.toString;
  }

  case class BigDecimalToken(v:BigDecimal) extends NumberToken
  {
   val value = v;
   def chars = value.toString;
  }


  case class TermWareErrorToken(s:String) extends TermWareToken
  {
   val chars=s;
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
