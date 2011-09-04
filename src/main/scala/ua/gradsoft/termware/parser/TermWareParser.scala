package ua.gradsoft.termware;

import scala.util.parsing.combinator.syntactical._;
import scala.util.parsing.input._;
import ua.gradsoft.termware._;
import ua.gradsoft.termware.flow._;
import ua.gradsoft.termware.parser._;

class TermWareParser(th:Theory, fname:String) extends TokenParsers
                                    with TermWareTokens
                                    with TheoryTermConversions
{
  override type Tokens = TermWareLexical;
  override val lexical: TermWareLexical = 
                              new TermWareLexical(th.operatorSyntax);

  import TokenType._;

  implicit def char2Delim(ch:Char):Parser[Token] = 
    elem("delimiter", (x:Elem)=>(
                         x.isInstanceOf[D]
                        &&
                         x.asInstanceOf[D].chars==ch.toString
                                )
                       ) ^^ {  _.asInstanceOf[D];  }

  implicit def str2Keyword(s:String):Parser[Token] =
    elem("keyword", (x:Elem) => (
                         x.isInstanceOf[KeywordToken]
                        &&
                         x.asInstanceOf[KeywordToken].chars==s
                       )) ^^ {  _.asInstanceOf[KeywordToken]; }


  def statements:Parser[List[Term]] = rep(statement);

  def statement:Parser[Term] = (
     packagedef
    |
     syntaxdef
    |
     term <~ opt(';')
  );

  def packagedef:Parser[Term] = ("package" ~! term ~
                       opt('{' ~> rep(statement) <~ '}')) ^^
    { case x ~ y ~ z => createPackage(y,z); }

  // syntax: plus as binary operator "+" (assoc right, priority 5) ;
  // syntax: invert as unary operator "~" ;
  def syntaxdef:Parser[Term] = 
        "syntax:" ~! identifier ~ syntaxOperatorDef <~ ';'  ^^
        {
         case x ~ y ~ z => {
            var retval:Term = null;
            if (z.arity==2) {
              val z2 = z.asInstanceOf[BinaryOperator];
              lexical.syntax.addBinary(z.sign,y.name.string,
                                       z2.leftAssociative,z2.priority);
              retval = th.funSignature("syntaxOperator").createTerm(
                    "syntaxOperator",
                    th.intSignature.createConstant(z.arity),
                    th.stringSignature.createConstant(z.sign),
                    th.stringSignature.createConstant(y.name.string),
                    th.booleanSignature.createConstant(z2.leftAssociative),
                    th.intSignature.createConstant(z2.priority)
              );
            } else if (z.arity==1) {
              lexical.syntax.addUnary(z.sign,y.name.string);
              retval = th.funSignature("syntaxOperator").createTerm(
                    "syntaxOperator",
                    th.intSignature.createConstant(z.arity),
                    th.stringSignature.createConstant(z.sign),
                    th.stringSignature.createConstant(y.name.string)
              );
            } else {
              throw new IllegalArgumentException("arity of operation must be 1 or 2");
            }
            lexical.clearOperators;
            retval;
         }
      }


  def syntaxOperatorDef:Parser[Operator] = (
    ("binary" ~> "operator" ~> stringLiteral ~
                                   opt(syntaxOperatorAttributes) ) ^^ {
                   case x ~ y => {
                     var leftAssoc = true;
                     var priority = 5;
                     if (y!=None) {
                       y.get.foreach({
                          (x:(String,Any)) => x._1 match {
                             case "leftAssoc" => leftAssoc = (x._2=="left");
                             case "priority" => priority = x._2.asInstanceOf[Term].getInt;
                          }
                       });
                     }
                     BinaryOperator(x.getString,"TMP",leftAssoc,priority);
                   }
                 }
    |
    ("unary" ~> "operator" ~> stringLiteral ) ^^ {
                 (x:Term) => UnaryOperator(x.asInstanceOf[Term].getString,
                                          "TMP");
              }
  );

  def syntaxOperatorAttributes:Parser[List[(String,Any)]] = 
   '(' ~> repsep(syntaxOperatorAttribute, ',') <~ ')';

  def syntaxOperatorAttribute:Parser[(String,Any)] = (
    "assoc" ~> ( "right" | "left" )
       ^^ { _ match {
              case KeywordToken("left") => ("leftAssoc", true);
              case KeywordToken("right") => ("leftAssoc", false);
          } }
   |
    "priority" ~> intLiteral ^^ { ("priority",_); }
  );

  def term:Parser[Term] = (
    ifElseTerm
    |
    letTerm
    |
    withTerm
    |
    binaryExpression(0) ~ ( opt(ruleTail) | opt(whereTail) ) ^^
     {
       case x ~ y => if (y==None) {
                       x;
                     } else {
                        //TODO: define apply/unapply for term
                      val n = y.get.name;
                       if (n==simpleRuleTailName) {
                           theory.createFunTerm(ruleName,x,y.get.subterm(0))
                       } else if (n==conditionalRuleTailName) {
                           theory.createFunTerm(conditionalRuleName,x, 
                                                  y.get.subterm(0))
                       } else if (n==whereTailName) {
                           theory.createFunTerm(whereName,x,y.get.subterm(0))
                       } else {
                           throw new IllegalArgumentException("impossible tail");
                      }
                     }
     }
  )

  def ifElseTerm:Parser[Term] = (
    "if" ~> '(' ~> term ~ (')' ~> term <~ "else") ~ term ^^ {
      case x ~ y ~ z => theory.createFunTerm(ifElseName,x,y,z); 
    }
  );

  def letTerm: Parser[Term] = (
    "let" ~> '(' ~> repsep(assign,',') ~ ( ')' ~> term ) ^^ {
          case x ~ y => if (x.isEmpty) y 
                          else {
                             val tx = termFromList(theory,x);
                             th.funSignature("let").createTerm("let",tx,y);
                          }
        }
  );

  def whereTail: Parser[Term] = (
     "where" ~> ( 
                  repsep(assign,',') 
                 |
                 '(' ~> repsep(assign,',') <~ ')' 
                )   ^^  {
                  x => th.createFunTerm(whereTailName,termFromList(theory,x));
                }
  );

  def assign: Parser[Term] = {
     identifier ~  opAssigment ~ term  ^^ {
         case x ~ y ~ z =>  th.funSignature("assign").createTerm("assign",x,y,z);
       }
  }

  def opAssigment: Parser[Term] = {
     (
      OP("<-")
      |
      OP("<-:")
      |
      OP("<-*")
     ) ^^ {
        x => IntTerm(1,th.intSignature);
     }
  }

  def ruleTail:Parser[Term] = (
      conditionalRuleTail
     |
      simpleRuleTail
  );

  def simpleRuleTail:Parser[Term] = (
     OP("->") ~> term  ^^  { cSimpleRuleTail(_); }
  );

  def OP(s:String): Parser[Token] = (
     elem("operator", (x:Elem)=>(
                         x.isInstanceOf[OperatorToken]
                        &&
                         x.asInstanceOf[OperatorToken].chars==s
                                )
                       ) ^^ {  _.asInstanceOf[OperatorToken];  }
  );


  def conditionalRuleTail:Parser[Term] = (
     rep1sep( conditionalRuleBranch , OP("|") ) ~ opt(OP("!->") ~> term ) ^^ {
       case x ~ y => {
          val branches = (if (y==None) 
                            x
                          else
                            x :+ theory.createFunTerm(conditionalRuleBranchName, 
                                           theory.booleanSignature.createConstant(true),
                                           y.get)
                         );
          theory.createFunTerm(conditionalRuleTailName, 
                                            termFromList(theory,branches));
       }
     }
  );

  def conditionalRuleBranch:Parser[Term] = (
       ('[' ~> term <~ ']') ~ OP("->") ~! term  ^^ {
           case x ~ y ~ z => theory.createFunTerm(
                                        conditionalRuleBranchName,x,z);
        }
  );

  def binaryExpression(p:Int):Parser[Term] = {
      if (p>=OperatorSyntax.MAX_BINARY_PRIORITY) {
        (
         unaryExpression ~ rep( binaryOperator(p)~unaryExpression )
              ^^ { case x ~ y => cBinaryExpression(x,y); }
        );
      } else {
         val pn = p+1;
         (
          binaryExpression(pn) ~ rep (binaryOperator(p) ~ binaryExpression(pn))
              ^^ { case x ~ y => cBinaryExpression(x,y); }
         );
      }
  }

  def unaryExpression:Parser[Term] = (
     opt(unaryOperator) ~ unarySuffixExpression
       ^^ { case x ~ y => cUnaryExpression(x,y); }
  );

  def unarySuffixExpression:Parser[Term] = (
      primaryExpression ~ opt( '(' ~> repsep(term,',') <~ ')' )
                ^^ { case x ~ y => cFunctional(x,y); }
  )


  def primaryExpression:Parser[Term] = (
        primitiveTerm
       |
        identifier 
       |
        '(' ~> term <~ ')'
       |
        '[' ~> repsep(term,',') <~ ']' ^^ { 
            (x):List[Term] => termFromList(theory,x); 
        }
  );

  def withTerm:Parser[Term] = (
       ("with" | '$' ) ~> '(' ~> repsep( vardef , ',' ) ~ ( ')' ~> term ) ^^ {
          case x ~ y => theory.withSignature.createTerm(withName,
                                   IndexedSeq(termFromList(theory,x),y));
       }
  );

  def vardef:Parser[Term] = (
        identifier ~ opt(':' ~> term) ^^ {
           case x ~ y  => { val typeTerm = if (y==None) typeTop else y.get;
                            theory.createFunTerm("vardef", x, typeTerm);
                          } 
        }
  );

  def binaryOperator(priority:Int):Parser[OperatorToken] =
    elem("binary operation with priority "+priority,
        (x:Elem) => ( 
              checkTokenType(x,OPERATOR)
             &&
              x.asInstanceOf[OperatorToken].v2!=null
             &&
              x.asInstanceOf[OperatorToken].v2.priority==priority)
        ) ^^ { _.asInstanceOf[OperatorToken]; }

  def unaryOperator:Parser[OperatorToken] =
    elem("unary operator",
        (x:Elem) => (
              checkTokenType(x,OPERATOR)
             &&
              x.asInstanceOf[OperatorToken].v1!=null)
        ) ^^ { _.asInstanceOf[OperatorToken]; } 
             

  def primitiveTerm:Parser[Term] = (
     elem("Boolean", checkTokenType(_,BOOLEAN)) ^^ {
                    x => cConstant[Boolean](theory.booleanSignature,x);
           }
    |
      stringLiteral
    |
     elem("Char", checkTokenType(_,CHAR)) ^^ {
                    x => cConstant[Char](theory.charSignature,x);
           } 
    |
     elem("Short", checkTokenType(_,SHORT)) ^^ {
                    x => cConstant[Short](theory.shortSignature,x);
           } 
    |
     intLiteral
    |
     elem("Long", checkTokenType(_,LONG)) ^^ {
                    x => cConstant[Long](theory.longSignature,x);
           }
    |
     elem("Double", checkTokenType(_,DOUBLE)) ^^ {
                    x => cConstant[Double](theory.doubleSignature,x);
           }
    |
     elem("Float", checkTokenType(_,FLOAT)) ^^ {
                    x => cConstant[Float](theory.floatSignature,x);
           }
    |
     elem("BigInt", checkTokenType(_,BIG_INT)) ^^ {
                    x => cConstant[BigInt](theory.bigIntSignature,x);
           }
    |
     elem("BigDecimal", checkTokenType(_,BIG_DECIMAL)) ^^ {
                    x => cConstant[BigDecimal](theory.bigDecimalSignature,x);
           }
    );


  def stringLiteral:Parser[Term] = 
     elem("String", checkTokenType(_, STRING)) ^^ {
                    x => cConstant[String](theory.stringSignature,x);
           } ;

  def intLiteral:Parser[Term] = 
     elem("Int", checkTokenType(_,INT)) ^^ {
                    x => cConstant[Int](theory.intSignature,x);
           } ;


  def identifier:Parser[Term] = (
     elem("Identifier", checkTokenType(_,IDENTIFIER)) ^^ {
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
                  ), x.asInstanceOf[Positional]);

  def cFunctional(x:Term, y:Option[List[Term]]):Term = y match {
    case Some(l) => cFunctional1(x,l);
    case None => x; 
  }

  def cFunctional1(x:Term, y:List[Term]):Term = {
    if (x.isAtom) {
       th.funSignature(x.name).createTerm(x.name,y.toSeq:_*); 
    } else {
       def listToTerm(l:List[Term]):Term = {
         if (l.isEmpty) th.nilSignature.createConstant(Nil)
         else th.funSignature("cons").createTerm("cons",
                                  l.head,listToTerm(l.drop(1)));
       }
       val ly = listToTerm(y);
       th.funSignature("apply").createTerm("apply",x,ly); 
    }
  }
  

  def cBinaryExpression(frs:Term, tail:List[~[OperatorToken,Term]]):Term =
  {
    if (tail.isEmpty) {
       return frs;
    }
    val opt = tail.head._1;
    val op = opt.v2;
    val snd = tail.head._2;
    val name = th.symbolTable.getOrCreate(op.funName);
    val retval:Term =  
       if (op.isRightAssoc) {
          val next = cBinaryExpression(snd,tail.drop(1));
          val par = IndexedSeq(frs,next);
          theory.createFunTerm(name,par:_*);
       } else {
          // leftAssoc
          // x * y * z * w = ((x * y) * z) * w
          val par = IndexedSeq(frs,snd);
          val next = th.funSignature(name).createTerm(name,par);
          cBinaryExpression(next,tail.drop(1));
       };
    return posAttributes(retval,opt);
  }

  def cUnaryExpression(frs:Option[OperatorToken],snd:Term):Term =
  {
    if (frs==None) snd;
    else {
      val op = frs.get.v1;
      val name = theory.symbolTable.getOrCreate(op.funName);
      theory.funSignature(name).createTerm(name, IndexedSeq(snd));
    }
  }

  def cSimpleRuleTail(t:Term) = theory.createFunTerm("ruleTail",t);
  
  def cSimpleConditionalTail(conds:Term, last:Term) = 
                    theory.createFunTerm("condRuleTail",conds,last);

  def createPackage(name:Term, internals:Option[List[Term]]):Term =
  {
   if (internals==None) {
      return theory.funSignature("_CurrentPackage").
                            createTerm("_CurrentPackage", name);
   } else {
      return theory.funSignature("_Package").
                            createTerm("_Package", name, 
                                    termFromList(theory, internals.get));
   }
  }

  def posAttributes(t:Term, x:Positional) = {
     t.setAttribute(POS,
          theory.refSignature.createConstant(
                            new PositionWithFname(x.pos,fileName))
        );
     t;
  }

  def checkTokenType(t:Elem, tokenType: TokenType.Value):Boolean =
      t match {
         case x: lexical.Token => x.tokenType == tokenType
         case _ => false;
      };

  lazy val typeTop = theory.typeAlgebra.top;

  lazy val ifElseName = theory.symbolTable.getOrCreate("ifElse");
  lazy val withName = theory.symbolTable.getOrCreate("with");
  lazy val ruleName = theory.symbolTable.getOrCreate("rule");
  lazy val simpleRuleTailName = theory.symbolTable.getOrCreate("ruleTail");
  lazy val conditionalRuleName = theory.symbolTable.getOrCreate("conditionalRule");
  lazy val conditionalRuleTailName = theory.symbolTable.getOrCreate("conditionalRuleTail");
  lazy val conditionalRuleBranchName = theory.symbolTable.getOrCreate("conditionalRuleBranchName");
  lazy val whereName = theory.symbolTable.getOrCreate("where");
  lazy val whereTailName = theory.symbolTable.getOrCreate("whereTail");


  val theory = th;
  val fileName = fname;
  lazy val POS = th.symbolTable.getOrCreate("POS");
}
