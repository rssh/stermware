package ua.gradsoft.termware;

import ua.gradsoft.termware.freeterms._;
import ua.gradsoft.termware.parser.OperatorSyntax;

trait FreeAlgebra extends Theory
                     with DefaultTermTranslation
{

  val booleanSignature = new BooleanTermSignature(this);
  val TRUE = booleanSignature.createConstant(true);
  val FALSE = booleanSignature.createConstant(false);

  lazy val freeAtomSignature = new AtomTermSignature(this,
                       TermWare.symbolTable.getOrCreate("FREE_ATOM"));

  def atomSignature(name:Name) = freeAtomSignature;


  val nilSignature = new NilTermSignature(this);  

  val charSignature = new CharTermSignature(this);
  val stringSignature = new StringTermSignature(this);

  val byteSignature = new ByteTermSignature(this);
  val shortSignature = new ShortTermSignature(this);
  val intSignature = new IntTermSignature(this);
  val longSignature = new LongTermSignature(this);
  val floatSignature = new FloatTermSignature(this);
  val doubleSignature = new DoubleTermSignature(this);
  val bigIntSignature = new BigIntTermSignature(this);
  val bigDecimalSignature = new BigDecimalTermSignature(this);

  val etaSignature = new EtaTermSignature(this);
  val xSignature = new XTermSignature(this);
  val refSignature = new RefTermSignature(this);
  val listSignature = new ListTermSignature(this);
  val arraySignature = new FreeTermWithFixedNameAnyAritySignature(
                                    symbolTable.getOrCreate("ARRAY"),
                                    this);

  val withSignature = new WithTermSignature(this);

  lazy val letSignature = new LetTermSignature(this);

  def funSignature(name:Name): TermSignature 
               = if (name==symbolTable.CONS)  {
                    listSignature;
                 } else {
                    freeFunSignature;
                 }

  val freeFunSignature = new FreeFunctionalTermSignature(this);

  val errorSignature = new ErrorTermSignature(this);

  val operatorSyntax = new OperatorSyntax()
                               .addBinary("<-","assign",true,1)
                               .addBinary("->","rule",false,1)
                               .addBinary("||","or",true,2)
                               .addBinary("&&","and",true,3)
                               .addBinary("==","equal",true,4)
                               .addBinary("!=","notEqual",true,4)
                               .addBinary("<","less",true,5)
                               .addBinary("<=","lessEq",true,5)
                               .addBinary(">","greater",true,5)
                               .addBinary(">=","greater",true,5)
                               .addBinary("+","plus",true,6)
                               .addBinary("-","minus",true,6)
                               .addBinary("%","mod",true,6)
                               .addBinary("*","multiply",true,7)
                               .addBinary("/","divide",true,7)
                               .addUnary("-","minus")

}
