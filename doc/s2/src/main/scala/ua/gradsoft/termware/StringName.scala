package ua.gradsoft.termware;

final case class StringName[T <% {def toString:String}](v: T, k:Int) extends Name
{
 def kindIndex: Int = k
 def index: Int = 0;
 def string: String = v.toString;

 override def compare(that: Name):Int =
   if (kindIndex == that.kindIndex)
        string.compareTo(that.string);
   else
        kindIndex - that.kindIndex
   ;

}


