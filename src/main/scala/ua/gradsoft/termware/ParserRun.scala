package ua.gradsoft.termware;

/**
 *part of facade for language parsers.
 *ParserRun means parser, bound to some input.
 **/
trait ParserRun
{

  def nextTerm(): Either[String,Term];

  def eof: Boolean;

  def error: Option[String];

}
