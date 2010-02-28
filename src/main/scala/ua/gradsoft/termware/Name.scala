package ua.gradsoft.termware;

object NameKindIndex
{
 val forSName = 1;
 val forRef = 2;
 val forBigInt = 3;
 val forBigDecimal = 4;
 val forLong = 5;
}

trait Name extends Ordered[Name]
{
 def getKindIndex:  Int;
 def getIndex: Int;
 def getString: String;

 override def compare(that: Name):Int = 
   if (getKindIndex == that.getKindIndex)
        getIndex - that.getIndex
   else
        getKindIndex - that.getKindIndex
   ;

 override lazy val hashCode = getKindIndex+getIndex+getString.hashCode;
}

