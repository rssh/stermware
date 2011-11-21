package ua.gradsoft.termware;


/**
 * Conversion between scala/term objects in some theory.
 **/
trait TheoryTermConversions
{

  def termFromBoolean(th:Theory, b:Boolean):Term =
                   th.booleanSignature.createConstant(b);

  def booleanFromTerm(t:Term):Boolean={
   if (t.isBoolean) {
     t.getBoolean;
   } else {
     throw new TermWareException("Can't convert term to boolean");    
   }
  }

  def termFromString(th:Theory, s:String):Term =
                   th.stringSignature.createConstant(s);

  def stringFromTerm(t:Term):String={
   if (t.isString) {
     t.getString;
   } else {
     throw new TermWareException("Can't convert term to string");    
   }
  }

  def termFromInt(th:Theory, i:Int):Term = 
                  th.intSignature.createConstant(i);

  def intFromTerm(t:Term):Int={
   if (t.isInt) {
     t.getInt;
   } else if (t.isByte) {
     return t.getByte.toInt;
   } else if (t.isShort) {
     return t.getShort.toInt;
   } else if (t.isLong) {
     val l = t.getLong;
     if (l.toInt == l) {
       return l.toInt;
     } else {
       throw new TermWareException("Can't convert term to integer");    
     }
   } else if (t.isBigInt) {
     val bi = t.getBigInt;
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
      case x:java.lang.Byte => th.byteSignature.createConstant(x)
      case x:java.lang.Short => th.shortSignature.createConstant(x)
      case x:java.lang.Integer => th.intSignature.createConstant(x)
      case x:java.lang.Long => th.longSignature.createConstant(x)
      case x:BigInt => th.bigIntSignature.createConstant(x)
      case x:BigDecimal => th.bigDecimalSignature.createConstant(x)
      case x:java.math.BigDecimal => th.bigDecimalSignature.createConstant(x)
      case x:String => th.stringSignature.createConstant(x)
      case x:java.lang.Character => th.charSignature.createConstant(x)
      case x:Array[_] => {
         val newBody = new Array[Term](x.length);
         var i=0;
         while(i < x.length) {
           newBody(i) = termFromAny(th,x(i));
           i += 1;
         }
         return th.arraySignature.createSpecial(newBody);
      }
      case x:List[_] => {
         if (x.isEmpty) {
          return th.nilSignature.createSpecial();
         } else {
          val consName = th.listSignature.fixedName.get;
          return th.listSignature.createTerm(consName, 
                                            termFromAny(th,x.head), 
                                            termFromAnyRef(th,x.drop(1)));
         }
      }
      case _ => th.refSignature.createConstant(v);
    }
  }

  def anyRefFromTerm(th:Theory, t:Term):AnyRef={
   if (t.isByte) {
     return new java.lang.Byte(t.getByte);
   }else if (t.isShort) {
     return new java.lang.Short(t.getShort);
   }else if (t.isInt) {
     return new java.lang.Integer(t.getInt);
   }else if (t.isString) {
     return t.getString;
   }else if (t.isBigInt) {
     return t.getBigInt;
   }else if (t.isBigDecimal) {
     return t.getBigDecimal;
   }else if (t.isRef) {
     return t.getRef;
   }else if (th.arraySignature.fixedName != None &&
         t.name == th.arraySignature.fixedName.get) {
         val retval = new Array[AnyRef](t.arity);
         for(i <- 0 to t.arity) {
           retval(i) = anyRefFromTerm(th,t.subterm(i));
         }
         return retval;
   } else if (th.listSignature.fixedName != None &&
         t.name == th.listSignature.fixedName.get) {
         val consName = t.name;
         var curr = t;
         var retval = List[AnyRef]();
         while(!curr.isNil) {
            if (curr.arity==2 && curr.name == consName) {
                retval=retval:::List(anyRefFromTerm(th,curr.subterm(0)));
                curr=curr.subterm(1);
            } else {
              throw new TermWareException("Illegal list term");
            }
         }
         return retval;
   }else{
     throw new TermWareException("Can't convert term to reference");    
   }
  }

  def termFromAny(th:Theory, a:Any):Term =
  {
    //termFromAnyRef
    a match {
      case x:Byte => th.byteSignature.createConstant(x)
      case x:Short => th.shortSignature.createConstant(x)
      case x:Int => th.intSignature.createConstant(x)
      case x:Long => th.longSignature.createConstant(x)
      case x:Character => th.charSignature.createConstant(x)
      case x:AnyRef => termFromAnyRef(th,x)
      case x => /* impossible */
                  throw new TermWareException("Can't convert term from any: unknown type for "+x.toString);
    }
  }

  def termFromList(th:Theory, l:List[Term]):Term = 
      l.foldRight(th.nilSignature.createConstant(null))(
        { th.funSignature("cons").createTerm("cons",_,_); }
      );

  def indexedSeqFromTermList(th:Theory, h:Term):IndexedSeq[Term] =
  {
    val consName = th.symbolTable.CONS;
    var c = h;
    var seq = IndexedSeq[Term]();
    while(!c.isNil) {
       seq = seq :+ c.subterm(0);
       c=c.subterm(1);
    }
    seq;
  }

}
