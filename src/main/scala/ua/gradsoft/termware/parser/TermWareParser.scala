package ua.gradsoft.termware;

import scala.util.parsing.combinator.syntactical._;
import scala.util.parsing.input._;
import ua.gradsoft.termware._;
import ua.gradsoft.termware.parser._;

class TermWareParser(th:Theory, fname:String) extends TokenParsers
                                    with TermWareTokens
                                    with TermImplicitConversions
{
  type Tokens = TermWareTokens;
  override val lexical = new TermWareLexical;

  def term:Parser[Term] = 
        primitiveTerm
  ;

  def primitiveTerm:Parser[Term] = (
     elem("Boolean", _.isInstanceOf[BooleanToken]) ^^ {
                                   x  => posAttributes(
                                            BooleanTerm(
                                             x.asInstanceOf[BooleanToken].v,
                                             theory.booleanSignature
                                            ), x.asInstanceOf[BooleanToken] )
                               }
    |
     elem("String", _.isInstanceOf[StringToken]) ^^ {
                                 x  => posAttributes(
                                          StringTerm(
                                           x.asInstanceOf[StringToken].value,
                                           theory.stringSignature
                                            ), x.asInstanceOf[StringToken] )
                               } 
    |
     elem("Char", _.isInstanceOf[CharToken]) ^^ {
                                 x  => posAttributes(
                                        theory.charSignature.createConstant(
                                           x.asInstanceOf[CharToken].value
                                            ), x.asInstanceOf[CharToken] )
                               } 
    |
     elem("Double", _.isInstanceOf[ValueToken[Double]]) ^^ {
                           x  => posAttributes(
                                  theory.doubleSignature.createConstant(
                                     x.asInstanceOf[ValueToken[Double]].value
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
