package ua.gradsoft.termware;

import scala.collection.immutable.TreeMap;

object Substitution
{
  def empty = new Substitution(TreeMap.empty);
}

class Substitution(values: Map[Term,Term]) extends PartialFunction[Term, Term]
{

  def +(kv:(Term,Term)): (Boolean,Substitution) = {
    val r = v.get(kv._1);
    if (r==None) 
       (true,new Substitution(v+kv))
     else 
       r.get.termUnify(kv._2,new Substitution(v+kv));
  } 

  override def isDefinedAt(t:Term) = v.isDefinedAt(t);
  override def apply(t:Term):Term = values.apply(t);

  val v = values;
};
