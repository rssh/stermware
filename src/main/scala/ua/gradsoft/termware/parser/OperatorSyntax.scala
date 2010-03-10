package ua.gradsoft.termware.parser;

import scala.collection.mutable._;

abstract class Operator(s:String, fn: String)
{

  def arity:Int;

  val sign = s;
  val funName = fn;
}

/**
 *Binary Operator 
 *@param s - string representation of operator (i. e. "+")
 *@param fn - appropriative functional symbol (i. e. "plus")
 *@param la - left associativity.
 *@param p - priority
 **/
case class BinaryOperator(s:String,fn:String,la:Boolean,p:Int)
                                                 extends Operator(s,fn)
{
 override def arity = 2;

 val leftAssociative = la;
 val priority = p;
 def isLeftAssoc = leftAssociative;
 def isRightAssoc = ! leftAssociative;
}

/**
 * Unary operator.
 **/
case class UnaryOperator(s:String,fun:String)
                                                 extends Operator(s,fun)
{
 override def arity = 1;
}

object OperatorSyntax
{
 val MAX_BINARY_PRIORITY=20;
}

/**
 * OperatorSyntax: set od binary or unary operators,
 * used in given theory
 **/
class OperatorSyntax {

  def addBinary(op:String,fun:String,la:Boolean,p:Int):this.type =
   {  if (p<0 || p > OperatorSyntax.MAX_BINARY_PRIORITY) {
        throw new IllegalArgumentException("priority is out of range");
      }
      binaries += (op -> BinaryOperator(op,fun,la,p)); 
      return this; }
    
  def removeBinary(op:String):this.type =
     {  binaries -= op; return this; }

  def getBinary(op:String) = binaries(op);

  def getBinaries = binaries;

  def addUnary(op:String, fun:String): this.type =
     { unaries += (op -> UnaryOperator(op,fun)); return this; }

  def removeUnary(op:String): this.type =
      {  unaries -= op; return this; }
                           
  def getUnaries = unaries;

  val binaries = new HashMap[String,BinaryOperator]();
  val unaries = new HashMap[String,UnaryOperator]();
}
