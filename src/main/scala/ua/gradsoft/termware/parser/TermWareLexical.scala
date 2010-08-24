package ua.gradsoft.termware.parser;

import ua.gradsoft.termware._;
import scala.util.parsing.combinator.lexical.Lexical;
import scala.util.parsing.input.CharArrayReader.EofCh

object TermWareLexical extends Enumeration
{
  type State = Value;
  val NORMAL,  IN_SYNTAX = Value;
}


class TermWareLexical(s:OperatorSyntax) extends Lexical
                          with TermWareTokens
{

  override type Token = TermWareToken;
  import TokenType._;
  import TermWareLexical._;

  def token1: Parser[Token] = positioned(
        primitive
       |
        delimiter
       |
        operator
       |
        keyword
       |
        identifier
  );

  def token: Parser[Token] = 
        token1 ^^ { 
     (x) => { if (true) {System.out.println(x.tokenType+":"+x.chars); };  x }
  }

  def identifier: Parser[Token] = (
    letter ~ rep( letter | digit )  ^^ {
             case  x ~ y => ValueToken[String](IDENTIFIER, (x::y) mkString "");
           }
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
       't'~'r'~'u'~'e'      ^^^ { ValueToken[Boolean](BOOLEAN,true); }
       |
       'f'~'a'~'l'~'s'~'e'  ^^^ { ValueToken[Boolean](BOOLEAN,false); } 
  );

  def stringPrimitive: Parser[Token] = (
       '\"'~! rep(stringInternals) ~! '\"' ^^ { 
                    case x ~ y ~ z => ValueToken[String](STRING,y mkString ""); 
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
   '\'' ~> charInternals <~ '\'' ^^ { x => ValueToken[Char](CHAR,x); }
  );

  def charInternals:Parser[Char] = (
     '\\' ~> stringEscape
     |
     chrExcept('\'',EofCh)
  );

  def numberPrimitive:Parser[Token] = (
     '0' ~ nonDecimalNumberPrimitive  ^^ { case x ~ y => y }
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
      decimalDigit ~! rep(decimalDigit) 
           ~ opt('.' ~> rep(decimalDigit)) ~ opt(numberSuffix)
              ^^ { case x ~ y ~ z ~ w  => createNumberToken(x::y,z,w,10); }
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

  private var operatorParsers:Parser[OperatorToken] = null;

  def operator:Parser[Token]= {
    if (operatorParsers==null) {
        operatorParsers = buildOperatorParsers;
    }
    operatorParsers;
  }

  def clearOperators = {
     operatorParsers = null;
  }

  private def buildOperatorParsers:Parser[OperatorToken] = {

    def parseOpString(s:String):Parser[OperatorToken] = (
      accept(s.toList) ^^ { x => new OperatorToken(
                                   syntax.getBinaries.getOrElse(s,null),
                                   syntax.getUnaries.getOrElse(s,null)
                                              );
                          }
    );

    (syntax.getBinaries.keys ++ syntax.getUnaries.keys).toList.
       distinct.
          sortWith((x,y)=>x.length > y.length).
             map(parseOpString(_)).reduceRight(_|_);
    
  }

  def keyword:Parser[Token] = {
    if (normalStateKeywordParser==null) {
      normalStateKeywordParser=buildKeywordParser(normalStateKeywords);
    }
    if (syntaxStateKeywordParser==null) {
      syntaxStateKeywordParser=buildKeywordParser(syntaxStateKeywords);
    }
    state match {
      case NORMAL => normalStateKeywordParser;
      case IN_SYNTAX => syntaxStateKeywordParser;
    }
  }

  case class KeywordDef(s:String,ns:Option[TermWareLexical.State])
  {
    val keyword = s;
    val nextState = ns;
  };

  private var normalStateKeywords: List[KeywordDef] = List(
           KeywordDef("syntax:",Some(IN_SYNTAX)),
           KeywordDef("if",None),
           KeywordDef("else",None),
           KeywordDef("let",None),
           KeywordDef("where",None),
           KeywordDef("whith",None)
  );
  private var normalStateKeywordParser: Parser[KeywordToken] = null;

  private var syntaxStateKeywords: List[KeywordDef] = List(
           KeywordDef("unary",None),
           KeywordDef("binary",None),
           KeywordDef("operator",None),
           KeywordDef("assoc",None),
           KeywordDef("left",None),
           KeywordDef("right",None),
           KeywordDef("priority",None)
  );
  private var syntaxStateKeywordParser:Parser[KeywordToken] = null;

  private def buildKeywordParser(keywords:List[KeywordDef]):Parser[KeywordToken]={
     
    def parseKeywordString(k:KeywordDef):Parser[KeywordToken] = (
      accept(k.keyword.toList) ^^ { 
                                   (x) => 
                                     if (k.nextState!=None) {
                                       state = k.nextState.get;
                                     }
                                     KeywordToken(k.keyword); 
                                   }
    );

    keywords.sortWith((x,y)=>x.keyword.length > y.keyword.length).
             map(parseKeywordString(_)).reduceRight(_|_);

  }

  def createNumberToken(intPart:List[Char],fractPart:Option[List[Char]],
                        suffix:Option[Char], radix:Int):Token =
  {
    if (fractPart==None) {
      val bi = new BigInt(new java.math.BigInteger(intPart.mkString(""),radix));
      if (suffix==None) {
        if (bi == bi.intValue) {
           ValueToken[Int](INT,bi.intValue);
        } else if (bi == bi.longValue) {
           ValueToken[Long](LONG,bi.longValue);
        } else {
           ValueToken[BigInt](BIG_INT,bi);
        }
      } else {
        suffix.get.toUpper match {
          case 'L' => ValueToken[Long](LONG,bi.longValue);
          case 'S' => ValueToken[Short](SHORT,bi.shortValue);
          case 'B' => ValueToken[BigInt](BIG_INT,bi);
          case 'D' => ValueToken[BigDecimal](BIG_DECIMAL,
                          new BigDecimal(
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
          ValueToken[Double](DOUBLE, bd.doubleValue);
        } else {
          suffix.get.toUpper match {
            case 'L' => ValueToken[Double](DOUBLE, bd.doubleValue);
            case 'S' => ValueToken[Float](FLOAT, bd.floatValue);
            case 'D' => ValueToken[BigDecimal](BIG_DECIMAL, new BigDecimal(bd));
            case _   => TermWareErrorToken("Invalid number suffix:"+suffix.get);
          } 
        } 
      }
    }
  }
                        

  def delimiter:Parser[Token]=(
       '{'  ^^^ { D("{"); }
      |
       '}'  ^^^ { D("}"); }
      |
       ','  ^^^ { D(","); }
      |
       '('  ^^^ { D("("); }
      |
       ')'  ^^^ { D(")"); }
      |
       ';'  ^^ { x => { if (state!=NORMAL) 
                          state=NORMAL; 
                        D(";"); 
                      }
                }
  );
  
  def whitespace: Parser[Any] = rep(
    whitespaceChar
    |
    comment
  );

  def comment: Parser[Any] = '#' ~ rep(chrExcept('\n')) ;

  val syntax = s;
  private var state = TermWareLexical.NORMAL;

}

