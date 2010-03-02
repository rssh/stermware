package ua.gradsoft.termware;

import scala.util.parsing.combinator.syntactical._;
import scala.util.parsing.input._;
import ua.gradsoft.termware._;
import ua.gradsoft.termware.parser._;

class TermWareParser(th:Theory, fname:String) extends TokenParsers
                                    with TermWareTokens
                                    with TermImplicitConversions
{
  override type Tokens = TermWareLexical;
  override val lexical: TermWareLexical = new TermWareLexical;
  type T = TermWareToken;

  import TokenType._;

  def term:Parser[Term] = 
        primitiveTerm
  ;

  def primitiveTerm:Parser[Term] = (
     elem("Boolean", _.asInstanceOf[T].tokenType==BOOLEAN ) ^^ {
                    x => cConstant[Boolean](theory.booleanSignature,x);
           }
    |
     elem("String", _.asInstanceOf[T].tokenType==STRING) ^^ {
                    x => cConstant[String](theory.stringSignature,x);
           } 
    |
     elem("Char", _.asInstanceOf[T].tokenType==CHAR) ^^ {
                    x => cConstant[Char](theory.charSignature,x);
           } 
    |
     elem("Short", _.asInstanceOf[T].tokenType==SHORT) ^^ {
                    x => cConstant[Short](theory.shortSignature,x);
           } 
    |
     elem("Int", _.asInstanceOf[T].tokenType==INT) ^^ {
                    x => cConstant[Int](theory.intSignature,x);
           }
     |
     elem("Long", _.asInstanceOf[T].tokenType==LONG) ^^ {
                    x => cConstant[Long](theory.longSignature,x);
           }
    |
     elem("Double", _.asInstanceOf[T].tokenType==DOUBLE) ^^ {
                    x => cConstant[Double](theory.doubleSignature,x);
           }
    |
     elem("Float", _.asInstanceOf[T].tokenType==FLOAT) ^^ {
                    x => cConstant[Float](theory.floatSignature,x);
           }
    |
     elem("BigInt", _.asInstanceOf[T].tokenType==BIG_INT) ^^ {
                    x => cConstant[BigInt](theory.bigIntSignature,x);
           }
    |
     elem("BigDecimal", _.asInstanceOf[T].tokenType==BIG_DECIMAL) ^^ {
                    x => cConstant[BigDecimal](theory.bigDecimalSignature,x);
           }
    );
  
  def cConstant[T](s:TermSignature,x:Elem):Term =
    posAttributes(s.createConstant(
                         x.asInstanceOf[ValueToken[T]].value
                  ).get, x.asInstanceOf[Positional]);

  def posAttributes(t:Term, x:Positional) = {
     t.setAttribute(POS,new PositionWithFname(x.pos,fileName));
     t;
  }

  val theory = th;
  val fileName = fname;
  lazy val POS = th.symbolTable.getOrCreate("POS");
}
