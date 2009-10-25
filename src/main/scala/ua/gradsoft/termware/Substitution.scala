package ua.gradsoft.termware;

import scala.collection.immutable.IntMap;

object Substitution
{
  def empty = new Substitution(IntMap.empty);
}

class Substitution(values: Map[Int,Term])
{

  def +(kv:(Int,Term)): Option[Substitution] = {
    val r = v.get(kv._1);
    if (r==None) Some(new Substitution(v+kv))
     else r.get.termUnify(kv._2,new Substitution(v+kv));
  } 

  val v = values;
};
