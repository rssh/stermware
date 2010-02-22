package ua.gradsoft.termware;

trait FreeAlgebra extends Theory
{

  val booleanSignature = new BooleanTermSignature(this);
  val TRUE = booleanSignature.createConstant(true);
  val FALSE = booleanSignature.createConstant(false);

  lazy val freeAtomSignature = new AtomTermSignature(this,
                       TermWare.symbolTable.getOrCreateElement("FREE_ATOM"));

  def atomSignature(name:Name) = freeAtomSignature;


  val nilSignature = new NilTermSignature(this);  

  val etaSignature = new EtaTermSignature(this);
  val etaXSignature = new EtaXTermSignature(this);

  def funSignature(name:Name, args:RandomAccessSeq[Term]): TermSignature 
                                                          = freeFunSignature;

  val freeFunSignature = new FreeFunctionalTermSignature(this);

  val errorSignature = new ErrorTermSignature(this);


}
