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
                         x  => posAttributes(
                                    BooleanTerm(
                                     x.asInstanceOf[ValueToken[Boolean]].v,
                                     theory.booleanSignature
                                    ), x.asInstanceOf[Positional] )
                               }
    |
     elem("String", _.asInstanceOf[T].tokenType==STRING) ^^ {
                        x  => posAttributes(
                                    StringTerm(
                                      x.asInstanceOf[ValueToken[String]].value,
                                      theory.stringSignature
                                    ), x.asInstanceOf[Positional] )
                        } 
    |
     elem("Char", _.asInstanceOf[T].tokenType==CHAR) ^^ {
                                 x  => posAttributes(
                                        theory.charSignature.createConstant(
                                           x.asInstanceOf[CharToken].value
                                            ), x.asInstanceOf[CharToken] )
                               } 
    |
     elem("Double", _.asInstanceOf[T].tokenType==DOUBLE) ^^ {
                           x  => posAttributes(
                                  theory.doubleSignature.createConstant(
                                     x.asInstanceOf[ValueToken[Double]].value
                                  ), x.asInstanceOf[Positional] )
                         }
    |
     elem("Float", _.asInstanceOf[T].tokenType==FLOAT) ^^ {
                           x  => posAttributes(
                                  theory.floatSignature.createConstant(
                                     x.asInstanceOf[ValueToken[Float]].value
                                  ), x.asInstanceOf[Positional] )
                         }
    );
  

  def posAttributes(t:Term, x:Positional) = {
     t.setAttribute(POS,new PositionWithFname(x.pos,fileName));
     t;
  }

  val theory = th;
  val fileName = fname;
  lazy val POS = th.symbolTable.getOrCreate("POS");
}
