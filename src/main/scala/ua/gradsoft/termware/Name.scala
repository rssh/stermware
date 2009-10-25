package ua.gradsoft.termware;

trait Name extends Comparable[Name]
{
 def getKindIndex:  Int;
 def getIndex: Int;
 def getString: String;

 override def compareTo(that: Name):Int = 
   if (getKindIndex == that.getKindIndex)
        getIndex - that.getIndex
   else
        getKindIndex - that.getKindIndex
   ;
}


