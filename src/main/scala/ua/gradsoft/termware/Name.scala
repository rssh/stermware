package ua.gradsoft.termware;

object NameKindIndex extends Enumeration
{
 val SNAME, CHAR, STRING, REF, BIG_INT, BIG_DECIMAL, LONG, INT = Value;
}

trait Name extends Ordered[Name]
{
 def getKindIndex:  Int;
 def getIndex: Int;
 def getString: String;
}

