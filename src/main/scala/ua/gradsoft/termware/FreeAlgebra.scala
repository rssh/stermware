package ua.gradsoft.termware;

import ua.gradsoft.termware.freeterms._;

trait FreeAlgebra extends Theory
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
  val etaXSignature = new EtaXTermSignature(this);
  val refSignature = new RefTermSignature(this);
  val listSignature = new ListTermSignature(this);
  val arraySignature = new FreeTermWithFixedNameAnyAritySignature(
                                    symbolTable.getOrCreate("ARRAY"),
                                    this);

  def funSignature(name:Name, args:RandomAccessSeq[Term]): TermSignature 
                                                          = freeFunSignature;

  val freeFunSignature = new FreeFunctionalTermSignature(this);

  val errorSignature = new ErrorTermSignature(this);



}
