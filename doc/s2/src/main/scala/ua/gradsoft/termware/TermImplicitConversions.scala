package ua.gradsoft.termware;

import java.math.BigInteger;

/**
 * trait in which implicit conversions between terms and scala/java objects
 * are defined. 
 **/
trait TermImplicitConversions
            extends TheoryTermConversions
{

  implicit def fromBoolean(b:Boolean):Term =
                                termFromBoolean(theory,b);

  implicit def toBoolean(t:Term):Boolean=
                                 booleanFromTerm(t);

  implicit def fromString(s:String):Term =
                                termFromString(theory,s);

  implicit def toString(t:Term):String=
                                stringFromTerm(t);

  implicit def fromInt(i:Int):Term = 
                                termFromInt(theory,i);
                                
  implicit def toInt(t:Term):Int=
                                intFromTerm(t);

  implicit def fromAnyRef(v: AnyRef): Term = 
                                termFromAnyRef(theory,v);

  implicit def toAnyRef(t:Term):AnyRef=
                                anyRefFromTerm(theory,t);

  def theory: Theory;

}
