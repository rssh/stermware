package ua.gradsoft.termware;

import scala.collection.mutable.ArrayStack;
import ua.gradsoft.termware.fn.FnMarker;

class VM
{

  def pushCommand(c:VM=>VM):Unit = commandsStack.push(c);

  def pushCommands(cs:(VM=>VM)*):Unit = 
   cs.foldLeft(())((x:Unit,y:VM=>VM)=>pushCommand(y));

  def pushCommandsReverse(cs:(VM=>VM)*):Unit = 
   cs.foldRight(())((x:VM=>VM,y:Unit)=>pushCommand(x));
                 
  def popCommand = commandsStack.pop;

  def createAndPushMarker:Int = {
     markerGen=markerGen+1;
     pushCommand(new FnMarker(markerGen)); 
     return markerGen; 
  }

  def runByMarker(marker:Int):Unit = {
     do {
       val c = popCommand;
       c match {
         case FnMarker(x) =>
                if (x==marker) 
                    return;
         case _ => c.apply(this)
       }
     } while(!commandsStack.isEmpty)
     
  }

  def pushData(a:Any):Unit = dataStack.push(a); 
  def popData:Any = dataStack.pop;

  def pushData2(a:Any):Unit = dataStack2.push(a); 
  def popData2:Any = dataStack2.pop;

  private val dataStack = new ArrayStack[Any]();
  private val dataStack2 = new ArrayStack[Any];
  private val commandsStack = new ArrayStack[VM=>VM];
  private var markerGen = 0;
}
