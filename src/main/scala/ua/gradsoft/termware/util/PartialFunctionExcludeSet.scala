package ua.gradsoft.termware.util;

import scala.collection.Set;

class PartialFunctionExcludeSet[A,B,C<%A](
                          origin: PartialFunction[A,B],
                          excludedIndexes: Set[C] 
                                      ) extends PartialFunction[A,B]
{

 override def isDefinedAt(index:A) =
   o.isDefinedAt(index) && (index match {
                             case x:C =>  ! e.contains(x)
                             case _ => true
                           })

 override def apply(index:A) = o.apply(index);

 private val e = excludedIndexes;
 private val o = origin;
}


