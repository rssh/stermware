package ua.gradsoft.termware;

import scala.collection.mutable.Map;
import scala.collection.mutable.HashMap;

trait TermAttributed
{

   def getAttribute(name:Name):Option[Term]
         = attributes.get(name);

   def setAttribute(name:Name,value:Term):Unit
         = attributes.update(name,value);

   def resetAttribute(name:Name):Option[Term]
        = attributes.remove(name);

   def attributes:Map[Name,Term];
}


