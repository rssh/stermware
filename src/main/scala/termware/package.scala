package termware

object `package` {

 def throwPE(s:String):Nothing = throw new ProcessingException(s)

 def throwPE(message:String, tracking: Seq[String]):Nothing = throw new ProcessingException(message, tracking)

}
