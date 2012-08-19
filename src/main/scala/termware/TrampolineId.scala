package termware;

import scala.util._

class TrampolineId(val x:Long) extends AnyVal

object TrampolineId 
{
   def next: TrampolineId =
   {
    val threadId = Thread.currentThread.getId
    val localId = local.value + 1
    local.value = localId
    new TrampolineId((threadId << 16) + localId)
   }

   private[this] val local = new DynamicVariable[Int](0)
    
}

