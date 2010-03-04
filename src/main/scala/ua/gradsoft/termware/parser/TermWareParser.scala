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

  implicit def char2Delim(ch:Char):Parser[Token] = 
    elem("delimiter", x => x==ch) ^^ { x => D(x.toString) };

  def term:Parser[Term] = binaryExpression(20);

  def binaryExpression(p:Int):Parser[Term] = {
      if (p==0) {
        (
         unaryExpression0 ~ rep( binaryOperator(0)~unaryExpression0 )
              ^^ { case x ~ y => cBinaryExpression(x,y); }
        );
      } else {
         val pn = p-1;
         (
          binaryExpression(pn) ~ rep (binaryOperator(p) ~ binaryExpression(pn))
              ^^ { case x ~ y => cBinaryExpression(x,y); }
         );
      }
  }


  def unaryExpression0:Parser[Term] = (
      primaryExpression ~ opt( '(' ~> repsep(term,',') <~ ')' )
                ^^ { case x ~ y => cFunctional(x,y); }
  )


  def primaryExpression:Parser[Term] = (
        primitiveTerm
       |
        identifier 
       |
        '(' ~> term <~ ')'
  );


  def binaryOperator(priority:Int):Parser[BinaryOperator] =
    elem("binary operation with priority "+priority,
        (x:Elem) => (
              x.isInstanceOf[T]
             &&
              x.asInstanceOf[T].tokenType==BINARY_OPERATOR
             &&
              x.asInstanceOf[BinaryOperator].priority==priority)
        ) ^^ { x => x.asInstanceOf[BinaryOperator]; }
             

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

  def identifier:Parser[Term] = (
     elem("Identifier", _.asInstanceOf[T].tokenType == IDENTIFIER) ^^ {
        x => {
           val xx = x.asInstanceOf[ValueToken[String]];
           val xxn = theory.symbolTable.getOrCreate(xx.value);
           posAttributes(
                theory.atomSignature(xxn).createConstant(xxn), xx);
        }
     }
  );
  
  def cConstant[T](s:TermSignature,x:Elem):Term =
    posAttributes(s.createConstant(
                         x.asInstanceOf[ValueToken[T]].value
                  ).get, x.asInstanceOf[Positional]);

  def cFunctional(x:Term, y:Option[List[Term]]):Term = y match {
    case Some(l) => cFunctional1(x,l);
    case None => x; 
  }

  def cFunctional1(x:Term, y:List[Term]):Term = {
    if (x.isAtom) {
       th.funSignature(name).createTerm(name,y); 
    } else {
       def listToTerm(l:List[Term]):Term = {
         if (l.isEmpty) th.nilSignature.createConstant(Nil)
         else th.funSignature("cons").createTerm("cons",
                                  l.first,listToTerm(l.drop(1)));
       }
       val ly = listToTerm(y);
       th.funSignature("apply").createTerm("apply",x,ly); 
    }
  }
  

  def cBinaryExpression(frs:Term, tail:List[~[BinaryOperator,Term]]):Term =
  {
    if (tail.isEmpty) {
       return frs;
    }
    val op = tail.head._1;
    val snd = tail.head._2;
    val name = th.symbolTable.getOrCreate(op.functionName);
    val retval =  
       if (op.isRightAssoc) {
          val next = cBinaryExpression(snd,tail.drop(1));
          val par = RandomAccessSeq(frs,next);
          th.funSignature(name).createTerm(name,par);
       } else {
          // leftAssoc
          // x * y * z * w = ((x * y) * z) * w
          val par = RandomAccessSeq(frs,snd);
          val next = th.funSignature(name).createTerm(name,par);
          cBinaryExpression(next,tail.drop(1));
       };
    return posAttributes(retval,op);
  }

  def posAttributes(t:Term, x:Positional) = {
     t.setAttribute(POS,new PositionWithFname(x.pos,fileName));
     t;
  }

  val theory = th;
  val fileName = fname;
  lazy val POS = th.symbolTable.getOrCreate("POS");
}
