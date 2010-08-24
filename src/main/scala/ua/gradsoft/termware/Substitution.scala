package ua.gradsoft.termware;

import scala.collection.immutable.TreeMap;


trait Substitution extends PartialFunction[Term,Term]
{
  def +(kv:(Term,Term)): (Boolean,Substitution) ;
}

object SimpleSubstitution
{
  def empty = new SimpleSubstitution(TreeMap.empty);
}

class SimpleSubstitution(val v: Map[Term,Term]) extends Substitution
{

  def +(kv:(Term,Term)): (Boolean,Substitution) = {
    val r = v.get(kv._1);
    if (r==None) 
       (true,new SimpleSubstitution(v+kv))
     else 
       r.get.termUnify(kv._2,new SimpleSubstitution(v+kv));
  } 

  override def isDefinedAt(t:Term) = v.isDefinedAt(t);
  override def apply(t:Term):Term = v.apply(t);

};


object STMSubstitution
{
  def empty = new STMSubstitution(TreeMap.empty,BigInt(1));
}


class STMSubstitution(val v: Map[Term,Pair[BigInt,Term]],
                      val lastZipIndex: BigInt) extends Substitution
{
  def withIndex(newZipIndex:BigInt):STMSubstitution = {
        val cmp = lastZipIndex.compare(newZipIndex);
        if (cmp > 0) 
             new STMSubstitution(v,newZipIndex)
        else if (cmp < 0)
             new STMSubstitution(
                   v.filter({ _._2._1 < newZipIndex }),newZipIndex)
        else /* newZipIndex == lastZipIndex */
             this;
  }
                    
  def +(kv:(Term,Term)): (Boolean,Substitution) = {
    val r = v.get(kv._1);
    val zkv = kv._1->Pair(lastZipIndex,kv._2);
    if (r==None) 
       (true,new STMSubstitution(v+zkv,lastZipIndex))
    else 
       r.get._2.termUnify(kv._2,new STMSubstitution(v+zkv,lastZipIndex));
  }

  override def isDefinedAt(t:Term) = v.isDefinedAt(t);
  override def apply(t:Term):Term = v.apply(t)._2;

}
