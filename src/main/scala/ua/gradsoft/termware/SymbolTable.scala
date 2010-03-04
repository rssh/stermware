package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import scala.collection.mutable.ArrayBuffer;

final class SName(value:String,index:Int) extends Name
{
 def getKindIndex: Int = NameKindIndex.SNAME.id;
 def getIndex: Int = i;
 def getString: String = v;

 override def compare(that: Name):Int =
   if (getKindIndex == that.getKindIndex)
        getIndex - that.getIndex
   else
        getKindIndex - that.getKindIndex
   ;


 private val v = value;
 private val i = index;
}


class SymbolTable 
{

  def getOrCreate(s: String): Name =
  {
    val r = byNames.get(s);
    if (r!=None) {
       return r.get;
    }
    this.synchronized {
      val r = byNames.get(s);
      if (r!=None) {
       return r.get;
      }
      val e=new SName(s,byIndexes.length); 
      byIndexes+=e;
      byNames+=(s->e);
      return e;
    } 
  }

  var byNames = new HashMap[String,SName];
  var byIndexes = new ArrayBuffer[SName];

  lazy val NIL = getOrCreate("nil");
  lazy val CONS = getOrCreate("cons");
  val TYPE = getOrCreate("TYPE");
  val ERROR = getOrCreate("ERROR");
  val ETA = getOrCreate("ETA");

}



