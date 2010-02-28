package ua.gradsoft.termware;

final case class StringName[T <% {def toString:String}](v: T, k:Int) extends Name
{
 def getKindIndex: Int = k
 def getIndex: Int = 0;
 def getString: String = value.toString;

 override def compare(that: Name):Int =
   if (getKindIndex == that.getKindIndex)
        getString.compareTo(that.getString);
   else
        getKindIndex - that.getKindIndex
   ;

 val value=v;
}


