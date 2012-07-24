package ua.gradsoft.termware.parser;

import scala.util.parsing.input._;

/**
 * structure which holsd information about position of term in input source.
 **/
case class PositionWithFname(pos:Position,fname:String) 
{
  val line = pos.line;
  val column = pos.column;
  val fileName = fname;
}
