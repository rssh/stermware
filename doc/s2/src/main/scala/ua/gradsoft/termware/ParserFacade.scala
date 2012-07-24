package ua.gradsoft.termware;

import java.io.Reader;

/**
 * Simple facade for language parsers
 **/
trait ParserFacade
{

   /**
    * parse text from reader.
    **/
   def parse(r:Reader): ParserRun;

   /**
    * parse from string.
    **/
   def parse(s:String): ParserRun;


   /**
    * shortcat for parsing.
    * i. e. parser("my text") instead parser.parse("my text")
    **/
   def apply(r:Reader) = parse(r);

   def apply(s:String) = parse(s);

}

