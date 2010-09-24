package ua.gradsoft.termware;


object ZipIndex
{

  def apply(t:Term,zi:BigInt):Tuple3[Option[Term],Option[Term],Int]=
  {
    if (zi == 1) {
      return (None,Some(t),0)
    }else{
      var bitIndex=1;
      var cup:Option[Term]=None;
      var ct:Option[Term]=Some(t);
      var cti=0;
      val bitLength=zi.bitLength;
      while(bitIndex < bitLength) {
        if (zi.testBit(bitLength-bitIndex-1)) {
           // right
           if (cup==None) {
              return (None,None,cti+1)
           } else {
              val cupg=cup.get;
              cti=cti+1;
              if (cti < cupg.arity) {
                ct=Some(cupg.subterms(cti));
              } else {
                return (cup,None,cti)
              }
           }
        }else{
           // down
           cup=ct;
           cti=0;
           if (ct.get.arity>0) {
             ct=Some(cup.get.subterms(cti)); 
           } else {
             return (cup,None,cti)
           }
        }
        bitIndex=bitIndex+1;
      }
      return (cup,ct,cti);
    }
  }

/*
  //precondition: up[i]=t.
  def down(up:Option[Term], st:Option[Term], i:Int) = 
    if (st==None) (None,None,0)
    else if (st.get.arity>0) (st, Some(st.get.subterm(0)), 0)
    else (st,None,0);

  def down(upl:List[Term], tl:List[Term], i:Int):(List[Term],List[Term],Int)={
    var rupl=List[Term]();
    var rtl=List[Term]();
    for(t <- tl) {
      if (t.arity>0) {
        rupl+=t;
        rtl+=t.subterm(0);
      }
    }
    return (rupl,rtl,0);
  }

  def right(up:Opt[Term], st:Opt[Term], i:Int) = 
     if (up==None) (None,None,i+1)
     else if (up.get.arity>=i) (up,None,i+1)
     else (up, Some(up.get.subterm(i+1)), i+1);

  def right(upl:List[Term], cl:List[Term], i:Int):(List[Term],List[Term],Int)={
    var rupl=upl;
    var rtl=List[Term]();
    val ni=i+1;
    for(up <- upl) {
      if (up.arity > ni) {
         rtl+=up.suberm(ni);
      }
    }
    return (rupl, rtl, ni);
  }
*/

}
