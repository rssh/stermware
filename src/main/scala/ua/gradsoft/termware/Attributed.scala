package ua.gradsoft.termware;

import scala.collection.mutable.Map;
import scala.collection.mutable.HashMap;

trait Attributed[T]
{

   def getAttribute(name:Name):Option[T]
         = attributes.get(name);

   def setAttribute(name:Name,value:T):Unit
         = attributes.update(name,value);

   def resetAttribute(name:Name):Option[T]
        = attributes.removeKey(name);

   def attributes:Map[Name,T];
}


