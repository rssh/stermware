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

  //precondition: up[i]=t.
  def down(up:Term, t:Term, i:Int) = (t, t.subterm(0), 0)

  def right(up:Term, t:Term, i:Int) = (t, t.subterm(i+1), i+1)

}
