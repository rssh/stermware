package ua.gradsoft.termware;

object NameKindIndex extends Enumeration
{
 val SNAME, CHAR, STRING, REF, 
    BIG_INT, BIG_DECIMAL, LONG, INT, FLOAT = Value;
}

trait Name extends Ordered[Name]
{
 def kindIndex:  Int;
 def index: Int;
 def string: String;
}

