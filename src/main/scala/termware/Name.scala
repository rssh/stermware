package termware

import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe.Type
import scala.reflect.runtime.universe.typeOf


sealed trait Name extends Ordered[Name]

case class SymbolName(s:Symbol) extends Name
{

   def compare(x:Name):Int =
    x match {
      case SymbolName(sx) => if (s eq sx) 0
                             else {
                               var c = s.hashCode - sx.hashCode
                               if (c!=0) {
                                  c
                               } else {
                                  s.toString.compare(sx.toString)
                               }
                             }
      case PrimitiveName(v) => -1
    }

}

case class PrimitiveName[V:TypeTag](v:V)(implicit ord:Ordering[V]) extends Name
{
 
   def valueType: Type = typeOf[V]

   def compare(x:Name):Int =
    x match {
      case SymbolName(_) => 1
      case xp@PrimitiveName(xv) =>
             var c = valueType.hashCode - xp.valueType.hashCode
             if (c != 0) 
               c
             else if (xp.valueType <:< valueType) 
               ord.compare(v, xv.asInstanceOf[V])
             else {
               // hashCodes are equals but types are differeent
               // very rare.
               c = valueType.toString.compare(xp.valueType.toString)  
               if ( c == 0) {
                  // very-very-very rare. near newer
                  c = v.hashCode - xp.v.hashCode
                  if (c == 0) {
                    c = v.toString.compare(xp.v.toString)
                  }
               }               
               c
             }
              
    }

}


// vim: set ts=4 sw=4 et:
