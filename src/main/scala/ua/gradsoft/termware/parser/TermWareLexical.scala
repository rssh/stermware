package ua.gradsoft.termware.parser;

import scala.util.parsing.combinator.lexical.Lexical;
import scala.util.parsing.input.CharArrayReader.EofCh


class TermWareLexical extends Lexical
                          with TermWareTokens
{
  type Token = TermWareToken;

  def token: Parser[Token] = positioned(
        primitive
       |
        delimiter
  );

  def primitive: Parser[Token] = (
       booleanPrimitive
      |
       stringPrimitive
      |
       charPrimitive
      | 
       numberPrimitive
  );

  def booleanPrimitive:Parser[Token] = (
       't'~'r'~'u'~'e'        ^^ { _ => BooleanToken(true); }
       |
       'f'~'a'~'l'~'s'~'e'    ^^ { _ => BooleanToken(false); } 
  );

  def stringPrimitive: Parser[Token] = (
       '\"'~! rep(stringInternals) ~! '\"' ^^ { 
                                 case x ~ y ~ z => StringToken(y mkString ""); 
                               }
      |
       '\"' ~>  failure("Unclosed string literal")
  );

  def stringInternals:Parser[Char] = (
     '\\' ~> stringEscape
     |
      chrExcept('\"',EofCh)  
  );


  def stringEscape:Parser[Char] = (
        accept('\"')
        |
        accept('\'')
        |
        '\\'
        |
        'n' ^^ { _ => '\n' }
        |
        't' ^^ { _ => '\t' }
        |
        'r' ^^ { _ => '\r' }
  );

  def charPrimitive:Parser[Token] = (
   '\'' ~> charInternals <~ '\'' ^^ { x => CharToken(x); }
  );

  def charInternals:Parser[Char] = (
     '\\' ~> stringEscape
     |
     chrExcept('\'',EofCh)
  );

  def numberPrimitive:Parser[Token] = (
     '0' ~! nonDecimalNumberPrimitive  ^^ { case x ~ y => y }
     |
      decimalNumberPrimitive
  );

  def nonDecimalNumberPrimitive:Parser[Token] = (
     'x' ~! rep(hexDigit) ~ opt('.' ~> rep(hexDigit)) ~ opt(numberSuffix)
              ^^ { case x ~ y ~ z ~ w => createNumberToken(y,z,w,16); }
    |
     'o' ~! rep(octDigit) ~ opt('.' ~> rep(octDigit)) ~ opt(numberSuffix)
              ^^ { case x ~ y ~ z ~ w => createNumberToken(y,z,w,8); }
    |
     failure("after 0 must be 'x' or 'o' in integer constant")
  );

  def decimalNumberPrimitive:Parser[Token] = (
      rep(decimalDigit) ~ opt('.' ~> rep(decimalDigit)) ~ opt(numberSuffix)
              ^^ { case x ~ y ~ z  => createNumberToken(x,y,z,10); }
  );

  def numberSuffix:Parser[Char] = (
   (accept('L')| 'l')
   |
   (accept('S') | 's')
   |
   (accept('D') | 'd')
   |
   (accept('B') | 'b')
   |
   ':' ~> longNumberSuffix
  );

  def longNumberSuffix:Parser[Char] = (
   (accept('L')| 'l')
   |
   (accept('S') | 's')
   |
   (accept('D') | 'd') 
   |
   (accept('B') | 'b')
  );

  def hexDigit:Parser[Char]=(
     accept('0') | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
   | accept('a') | 'A' | 'b' | 'B' | 'c' | 'C' | 'd' | 'D' | 'e' | 'E'
   | accept('f') | 'F'
  );

  def octDigit:Parser[Char]=(
     accept('0') | '1' | '2' | '3' | '4' | '5' | '6' | '7' 
  );

  def decimalDigit:Parser[Char]=(
     accept('0') | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
  );


  def createNumberToken(intPart:List[Char],fractPart:Option[List[Char]],
                        suffix:Option[Char], radix:Int):Token =
  {
    if (fractPart==None) {
      val bi = new BigInt(new java.math.BigInteger(intPart.mkString(""),radix));
      if (suffix==None) {
        if (bi == bi.intValue) {
           new IntToken(bi.intValue);
        } else if (bi == bi.longValue) {
           new LongToken(bi.intValue);
        } else {
           new BigIntToken(bi);
        }
      } else {
        suffix.get.toUpper match {
          case 'L' => new LongToken(bi.longValue);
          case 'S' => new ShortToken(bi.shortValue);
          case 'B' => new BigIntToken(bi);
          case 'D' => new BigDecimalToken(new BigDecimal(
                              new java.math.BigDecimal(bi.bigInteger)));
          case _   => TermWareErrorToken(
                          "Invalid integer number suffix:"+suffix.get);
        }
      }                                   
    } else {
      if (radix!=10) {
        TermWareErrorToken("non-decimal number with decimal or floation point is not allowed");
      }else{
        val scale = fractPart.get.length;
        val li =  intPart.mkString("")+fractPart.get.mkString("");
        val bi = new java.math.BigInteger(li);
        val bd = new java.math.BigDecimal(bi,scale);
        if (suffix==None) {
          ValueToken[Double](bd.doubleValue);
        } else {
          suffix.get.toUpper match {
            case 'L' => ValueToken[Double](bd.doubleValue);
            case 'S' => ValueToken[Float](bd.floatValue);
            case 'D' => new BigDecimalToken( new BigDecimal(bd) );
            case _   => TermWareErrorToken("Invalid number suffix:"+suffix.get);
          } 
        } 
      }
    }
  }
                        

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

  def comment: Parser[Any] = '#' ~ rep(chrExcept('\n')) ;

}

