package ua.gradsoft.termware;


/**
 * Conversion between scala/term objects in some theory.
 **/
trait TheoryTermConversions
{

  def termFromBoolean(th:Theory, b:Boolean):Term =
                   th.booleanSignature.createConstant(b).get;

  def booleanFromTerm(t:Term):Boolean={
   if (t.isBoolean) {
     t.getBoolean.get;
   } else {
     throw new TermWareException("Can't convert term to boolean");    
   }
  }

  def termFromString(th:Theory, s:String):Term =
                   th.stringSignature.createConstant(s).get;

  def stringFromTerm(t:Term):String={
   if (t.isString) {
     t.getString.get;
   } else {
     throw new TermWareException("Can't convert term to string");    
   }
  }

  def termFromInt(th:Theory, i:Int):Term = 
                  th.intSignature.createConstant(i).get;

  def intFromTerm(t:Term):Int={
   if (t.isInt) {
     t.getInt.get;
   } else if (t.isByte) {
     return t.getByte.get.toInt;
   } else if (t.isShort) {
     return t.getShort.get.toInt;
   } else if (t.isLong) {
     val l = t.getLong.get;
     if (l.toInt == l) {
       return l.toInt;
     } else {
       throw new TermWareException("Can't convert term to integer");    
     }
   } else if (t.isBigInt) {
     val bi = t.getBigInt.get;
     if (bi.toInt == bi) {
       return bi.toInt;
     } else {
       throw new TermWareException("Can't convert term to integer");    
     }
   } else {
     throw new TermWareException("Can't convert term to integer");    
   }
  }

  def termFromAnyRef(th:Theory, v: AnyRef): Term = {
    v match {
      case x:java.lang.Byte => th.byteSignature.createConstant(x).get
      case x:java.lang.Short => th.shortSignature.createConstant(x).get
      case x:java.lang.Integer => th.intSignature.createConstant(x).get
      case x:java.lang.Long => th.longSignature.createConstant(x).get
      case x:BigInt => th.bigIntSignature.createConstant(x).get
      case x:BigDecimal => th.bigDecimalSignature.createConstant(x).get
      case x:String => th.stringSignature.createConstant(x).get
      case x:java.lang.Character => th.charSignature.createConstant(x).get
      case x:Array[AnyRef] => {
         val newBody = new Array[Term](x.length);
         var i=0;
         while(i < x.length) {
           newBody(i) = termFromAnyRef(th,x(i));
           i += 1;
         }
         return th.arraySignature.createSpecial(newBody).get;
      }
      case x:List[AnyRef] => {
         if (x.isEmpty) {
          return th.nilSignature.createSpecial().get;
         } else {
          val consName = th.listSignature.fixedName.get;
          return th.listSignature.createTerm(consName, 
                                            termFromAnyRef(th,x.first), 
                                            termFromAnyRef(th,x.drop(1))).get;
         }
      }
      case _ => th.refSignature.createConstant(v).get;
    }
  }

  def anyRefFromTerm(th:Theory, t:Term):AnyRef={
   if (t.isByte) {
     return new java.lang.Byte(t.getByte.get);
   }else if (t.isShort) {
     return new java.lang.Short(t.getShort.get);
   }else if (t.isInt) {
     return new java.lang.Integer(t.getInt.get);
   }else if (t.isString) {
     return t.getString.get;
   }else if (t.isBigInt) {
     return t.getBigInt.get;
   }else if (t.isBigDecimal) {
     return t.getBigDecimal.get;
   }else if (t.isRef) {
     return t.getRef.get;
   }else if (th.arraySignature.fixedName != None &&
         t.name == th.arraySignature.fixedName.get) {
         val retval = new Array[AnyRef](t.arity);
         for(i <- 0 to t.arity) {
           retval(i) = anyRefFromTerm(th,t.subterm(i).get);
         }
         return retval;
   } else if (th.listSignature.fixedName != None &&
         t.name == th.listSignature.fixedName.get) {
         val consName = t.name;
         var curr = t;
         var retval = List[AnyRef]();
         while(!curr.isNil) {
            if (curr.arity==2 && curr.name == consName) {
                retval=retval:::List(anyRefFromTerm(th,curr.subterm(0).get));
                curr=curr.subterm(1).get;
            } else {
              throw new TermWareException("Illegal list term");
            }
         }
         return retval;
   }else{
     throw new TermWareException("Can't convert term to reference");    
   }
  }

}
