package ua.gradsoft.termware;

import scala.util.parsing.combinator.syntactical._;
import ua.gradsoft.termware._;
import ua.gradsoft.termware.parser._;

class TermWareParser(th:Theory) extends TokenParsers
                                    with TermWareTokens
{
  type Tokens = TermWareTokens;
  override val lexical = new TermWareLexical;

  def term:Parser[Term] = 
        primitiveTerm
  ;

  def primitiveTerm:Parser[Term] = 
     elem("Boolean", _.isInstanceOf[BooleanToken]) ^^ {
                                   x  => BooleanTerm(
                                          x.asInstanceOf[BooleanToken].v,
                                          theory.booleanSignature
                                         );
                               };
  


  val theory = th;
}
