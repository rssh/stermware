package ua.gradsoft.termware;


case class RefTerm(v:AnyRef, s: RefTermSignature) extends PrimitiveTerm(s)
                                           with NonNumberTerm
                                           with NonBooleanTerm
{

  	
	
  override def isRef: Boolean = true;

  override def getRef: AnyRef = _value;
  
  override def optValue[T](implicit mt:Manifest[T]):Option[T] = 
  {
  	if (mt <:< Manifest.Object) {
  	  // do approx	
  	   val cm: Class[_] = mt.erasure;
  	   val vm: Class[_] = _value.getClass();
  	   if (vm.isAssignableFrom(cm)) {
  	  	 Some(_value.asInstanceOf[T]) 
  	   } else {
  	  	 None 
  	   }	
  	} else None
  }

  def fixTermCompare(t: Term):Int = {
    var c = termClassIndex - t.termClassIndex;
    if (c!=0) return 0;
    if (!(_value eq null)) {
      if (_value eq t.getRef) {
         return 0;
      }
      c = _value.hashCode - t.getRef.hashCode;
      if (c!=0) return c;
      // equal hash codes with special names: near impossible, but can be specially constructed.
      // in such (very rare) case compare string representations.
      return _value.toString.compare(t.getRef.toString);
    } else {
      return if (t.getRef==null)  0 else -1 ;
    }
  }

  def fixTermEq(t: Term):Boolean = (fixTermCompare(t)==0);

  def termClassIndex: Int = TermClassIndex.REF;

  lazy val name = signature.theory.symbolTable.getOrCreate(
                                                      "@"+_value.hashCode
                                                          );

  lazy val termHashCode = v.hashCode;

  private[this] val _value = v;
}

