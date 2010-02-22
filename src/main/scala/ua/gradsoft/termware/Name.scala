package ua.gradsoft.termware;

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


