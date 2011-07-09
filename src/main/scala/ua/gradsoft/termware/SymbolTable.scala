package ua.gradsoft.termware;

import scala.collection.mutable.HashMap;
import scala.collection.mutable.ArrayBuffer;

final class SName(val v:String,val i:Int) extends Name
{
 def kindIndex: Int = NameKindIndex.SNAME.id;
 def index: Int = i;
 def string: String = v;

 override def compare(that: Name):Int =
   if (kindIndex == that.kindIndex)
        index - that.index
   else
        kindIndex - that.kindIndex
   ;

 override def toString = string;

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
  lazy val TYPE = getOrCreate("TYPE");
  lazy val ERROR = getOrCreate("ERROR");
  lazy val ETA = getOrCreate("ETA");
  lazy val WITH = getOrCreate("WITH");

}



