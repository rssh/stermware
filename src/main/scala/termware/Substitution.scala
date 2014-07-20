package termware;

import scala.collection.immutable.TreeMap
import scala.util._
import scala.reflect.runtime.universe._


trait Substitution extends PartialFunction[Term,Term]
{

  def isEmpty: Boolean

  def get(k:Term): Option[Term]

  def getOrElse(k:Term, default: Term): Term =
    get(k) match {
       case Some(x) => x
       case None    => default
    }

  def put(k:Term, v:Term): UnificationResult

  def merge(s: Substitution): UnificationResult
                          
  def withIndex(newZipIndex:BigInt): Substitution

  def lastZipIndex: BigInt

  def foreach(f:((Term,Term)) => Unit): Unit

  def find(f:((Term,Term))=>Boolean): Option[(Term,Term)]

}


object STMSubstitution
{
  def empty = new STMSubstitution(TreeMap[Term,Tuple2[BigInt,Term]](),BigInt(1));
}


case class STMSubstitution(val map: Map[Term,Tuple2[BigInt,Term]],
                      val lastZipIndex: BigInt)
                                        extends Substitution
{

  def withIndex(newZipIndex:BigInt):STMSubstitution = {
        val cmp = lastZipIndex.compare(newZipIndex);
        if (cmp > 0) 
             new STMSubstitution(map,newZipIndex)
        else if (cmp < 0)
             new STMSubstitution(
                   map.filter({ _._2._1 < newZipIndex }),newZipIndex)
        else /* newZipIndex == lastZipIndex */
             this;
  }

  def get(k:Term) = map.get(k) map (_._2)
                    
  def put(k:Term, v:Term): UnificationResult  = {
    map.get(k) match {
       case None => UnificationSuccess(STMSubstitution(map.updated(k,(lastZipIndex,v)),lastZipIndex))
       case Some((v1,i1)) =>
              v.unify(v1, this) flatMap (merge(_))
    }
  }


  def merge(s:Substitution): UnificationResult = {
    ???
  }

  override def find(f:((Term,Term))=>Boolean): Option[(Term,Term)] =
    map.mapValues(_._2).find(f)

  override def isEmpty = map.isEmpty

  override def isDefinedAt(t:Term) = map.isDefinedAt(t)

  override def apply(t:Term):Term = map(t)._2

  override def foreach(f:((Term,Term))=>Unit) = map.mapValues(_._2).foreach(f)


}


