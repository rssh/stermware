package ua.gradsoft.termware.parser;

import scala.util.parsing.combinator.lexical.Lexical;

class TermWareLexical extends Lexical
                          with TermWareTokens
{


  def token: Parser[Token] = (
        primitive
       |
        delimiter
  );

  def primitive: Parser[Token] = booleanPrimitive;

  def booleanPrimitive:Parser[Token] = (
       't'~'r'~'u'~'e'        ^^ { _ => BooleanToken(true); }
       |
       'f'~'a'~'l'~'s'~'e'    ^^ { _ => BooleanToken(false); } 
  );

  def delimiter:Parser[Token]=(
       '{'  ^^ { _ => D("{"); }
      |
       '}'  ^^ { _ => D("}"); }
  );
  
  def whitespace: Parser[Any] = rep(
    whitespaceChar
    |
    comment
  );

  def comment: Parser[Any] = '#' ~ chrExcept('\n') ;

}

