package ua.gradsoft.termware;


/**
 * translations from scala objects to terms.
 */
trait DefaultTermTranslation
{
   this: Theory =>

   def fromAnyRef(x:AnyRef):Term =
   {
     x match {
        case b: java.lang.Boolean => booleanSignature.createConstant(b)
        case s: java.lang.String => stringSignature.createConstant(s)
        case c: java.lang.Character => charSignature.createConstant(c)
        case b: java.lang.Byte => byteSignature.createConstant(b)
        case s: java.lang.Short => shortSignature.createConstant(s)
        case i: java.lang.Integer => intSignature.createConstant(i)
        case l: java.lang.Long => longSignature.createConstant(l)
        case d: java.lang.Double => doubleSignature.createConstant(d)
        case f: java.lang.Float => floatSignature.createConstant(f)
        case bd: BigDecimal => bigDecimalSignature.createConstant(bd)
        case bi: BigInt => bigIntSignature.createConstant(bi)
        case Nil => nilSignature.createConstant(Nil)
        case t: Term => t
        case _          => refSignature.createConstant(x)
     }
   }

   def fromAny(x:Any):Term =
   {
     x match {
        case b: Boolean => booleanSignature.createConstant(b)
        case s: String => stringSignature.createConstant(s)
        case c: Character => charSignature.createConstant(c)
        case b: Byte => byteSignature.createConstant(b)
        case s: Short => shortSignature.createConstant(s)
        case i: Int => intSignature.createConstant(i)
        case l: Long => longSignature.createConstant(l)
        case d: Double => doubleSignature.createConstant(d)
        case f: Float => floatSignature.createConstant(f)
        case bd: BigDecimal => bigDecimalSignature.createConstant(bd)
        case bi: BigInt => bigIntSignature.createConstant(bi)
        case Nil => nilSignature.createConstant(Nil)
        case t: Term => t
        case r: AnyRef => fromAnyRef(r)
        case _ => throw new TermWareException("Unknown primitive type:"+x)
     }
   }

// TODO: think about pluggable complex transformations.
//  (may be theory will hold instance of package prefixes
//   to match classes)
  
   
}
