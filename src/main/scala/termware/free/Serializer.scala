package termware.free

import java.nio._
import termware._
import termware.util._
import termware.TermAttributes.{empty=>emptyAttributes}

// TODO: write input pos on error.

trait Serializer extends TermSerializer
{

   def apply(t: Term, out: Output): Unit = 
   { writeTerm(t,BlockContext(),out) }

   def unapply(in: Input): Term =
    readTerm(in,BlockContext())._1

   def termSystem: TermSystem


   case class BlockContext(
     val structures: Map[Int, TermStructure] = Map(),
     val invStructures: Map[TermStructure, Int] = Map(),
     val structureIndex: Int = 0,
     val scopes:     Map[Int, Term] = Map(),
     val invScopes:  Map[Term,Int] = Map(),
     val scopeIndex: Int = 0
   ) {

     def withStructure(structure: TermStructure): BlockContext =
     {
       invStructures.get(structure) match {
         case None => val si = structureIndex+1
                      copy(structures = structures.updated(si, structure),
                      invStructures = invStructures.updated(structure,si),
                      structureIndex = si)
         case Some(si) => this
       }
     }


     def withScope(scope: Term): BlockContext =
     {
       invScopes.get(scope) match {
         case None =>
              val si = scopeIndex+1
              copy(scopes = scopes.updated(si,scope),
                   invScopes = invScopes.updated(scope,si),
                   scopeIndex = si)
         case Some(_) => this
       }
     }

   }

    
   final val TAG_TERM = 100
   final val TAG_TERMSTRUCTURE = 101
   final val TAG_ATTRIBUTES = 102

   final val TAG_END = 400
   
   

   final val TAG_ATOM = 1;
   final val TAG_STRUCTURED=2;
   final val TAG_VAR = 3;

   final val TAG_PRIMITIVE = 0x1000;


   private def writeTerm(t: Term, bc: BlockContext, out: Output): BlockContext =
   {
    val nbc0 = t match {
      case a: AtomTerm       => out << TAG_ATOM << a.value ; bc
      case p: PrimitiveTerm  => writePrimitive(p,out)
      case s: StructuredTerm => writeStructured(s,bc,out)
      case v: VarTerm        => writeVar(v,bc,out)
    }     
    writeAttributes(t.attributes,bc,out)
   }

   private def readTerm(in: Input, bc: BlockContext): (Term, BlockContext) =
   {
      val tag = in.readInt
      val (t,nbc0) = tag match {
        case TAG_ATOM => (AtomTerm(in.readString,Map(),termSystem),bc)
        case TAG_STRUCTURED => readStructured(in, bc)
        case TAG_TERMSTRUCTURE => readTerm(in,readAndAdoptTermStructure(in,bc)) 
        case TAG_VAR => (readVar(in,bc),bc)
        case x if ((x|TAG_PRIMITIVE)!=0) => (readPrimitive(x,in), bc)
        case _ => throw new IllegalStateException("Invalid term tag:"+tag)
      }
      val (attributes, nbc) = readAttributes(in,nbc0)
      (t withAttributes attributes, nbc)
   }


   private def writePrimitive(p: PrimitiveTerm, out: Output): Unit =
   {
     out << (TAG_PRIMITIVE | p.name.typeIndex)
     p match {
       case StringTerm(v,_,_) => out << v
       case CharTerm(v,_,_)   => out << v
       case Int32Term(v,_,_)  => out << v
       case Int64Term(v,_,_)  => out << v
       case DoubleTerm(v,_,_) => out << v
       case OpaqueTerm(v,_,_) => (out << v.size).write(v)
     }
   }

   private def readPrimitive(tag: Int, in: Input): PrimitiveTerm =
   {
      import NameTypeIndexes._
      (tag & (~TAG_PRIMITIVE)) match {
        case STRING =>  StringTerm(in.readString,Map(),termSystem)
        case CHAR   =>  CharTerm(in.readChar,Map(),termSystem)
        case INT    =>  Int32Term(in.readInt,Map(),termSystem)
        case LONG   =>  Int64Term(in.readLong,Map(),termSystem)
        case DOUBLE =>  DoubleTerm(in.readDouble(),Map(),termSystem) 
        case OPAQUE =>  OpaqueTerm(in.readOpaque(),Map(),termSystem)       
        case _ => throw new IllegalStateException("Invalid primitive tag: "+tag)
      }
   }
 
   private def writeStructured(t: StructuredTerm, bc: BlockContext, out: Output): BlockContext =
   {
      val ts = t.termStructure;
      val nbc0 = bc.invStructures.get(ts) match {
                  case Some(tsi) => bc
                  case None =>
                         val newTsi = bc.structureIndex+1
                         val newTs = bc.structures.updated(newTsi,ts)
                         val newInvTs = bc.invStructures.updated(ts, newTsi)
                         out.writeInt(TAG_TERMSTRUCTURE) 
                         writeTermStructure(ts,out)
                         bc.copy(structures=newTs, invStructures=newInvTs, structureIndex=newTsi)
      }
      val newScopeIndex = bc.scopeIndex+1
      val newScopes = bc.scopes.updated(newScopeIndex,t)
      val newInvScopes = bc.invScopes.updated(t,newScopeIndex)
      val nbc = nbc0.copy(scopes=newScopes, invScopes=newInvScopes, scopeIndex=newScopeIndex) 
      out.writeInt(TAG_STRUCTURED) 
      out.writeInt(nbc.structureIndex)
      out.writeInt(t.arity)
      t.components.foldLeft(nbc) { (s,e) =>
         writeTerm(e,s,out)
      } 
   }

   private def readStructured(in: Input, bc: BlockContext): (Term, BlockContext) =
   {
     val tsi = in.readInt
     val ts: TermStructure = bc.structures.getOrElse(tsi,throw new IllegalStateException(
                                           "invalid structure index "))
     val arity = in.readInt 
     val subterms = new Array[Term](arity)
     var cbc = bc
     for(i <- (0 until arity)) {
        val (t, nbc) = readTerm(in, cbc)
        subterms(i)=t
        cbc = nbc
     }
     (StructuredTerm(ts,subterms.toIndexedSeq,emptyAttributes,termSystem), cbc)
   }

   private def writeVar(t: VarTerm, bc: BlockContext, out: Output): BlockContext =
   {
     val scopeIndex = t.scope match {
                       case Some(u) => bc.invScopes.getOrElse(u,
                                        throw new IllegalStateException("scope not found in uplevel"))
                       case None => -1
                    }
     out.writeInt(scopeIndex)
     writeName(t.name, out)
     out.writeInt(t.index)
     bc
   }

   private def readVar(in: Input, bc: BlockContext): VarTerm = 
   {
     val scopeIndex = in.readInt
     // TODO: handle error when scopeIndex is invalid ?
     val scope: Option[Term] = if (scopeIndex == -1) None else bc.scopes.get(scopeIndex)
     val name = readName(in)                          
     val index = in.readInt()
     VarTerm(name,index,scope,Map(),termSystem)
   }

   def writeName(name:Name, out: Output): Unit = 
   {
    out.writeInt(name.typeIndex)
    name match {
      case AtomName(v) => out << v
      case StringName(v) => out << v
      case CharName(v) => out << v
      case LongName(v) => out << v
      case IntName(v) => out << v
      case DoubleName(v) => out << v
      case OpaqueName(v) => out << v
    }
   }


   def readName(in: Input): Name = 
   {
    import NameTypeIndexes._
    val typeIndex = in.readInt
    typeIndex match {
      case ATOM   => AtomName(in.readString)
      case STRING => StringName(in.readString)
      case CHAR   => CharName(in.readChar)
      case LONG   => LongName(in.readLong)
      case INT    => IntName(in.readInt)
      case DOUBLE => DoubleName(in.readDouble)
      case OPAQUE => OpaqueName(in.readOpaque)
    }
   }

   private def writeAttributes(attributes: Map[Name,Term], bc: BlockContext, out: Output): BlockContext = 
   {
     out.writeInt(attributes.size)
     attributes.foldLeft(bc){ (s,e) =>
       writeName(e._1,out)
       writeTerm(e._2,s,out)
     }
   }
   
   private def readAttributes(in: Input, bc: BlockContext): (Map[Name,Term], BlockContext) = 
   {
     val r: Map[Name,Term] = Map()
     val nAttributes = in.readInt
     (1 to nAttributes).foldLeft((r,bc)) { (s,i) =>
       val name = readName(in)
       val (t,nbc) = readTerm(in,s._2)
       (s._1.updated(name,t),nbc)
     }
   }

   private def writeTermStructure(ts: TermStructure, out: Output): Unit = 
       TermStructure.write(ts,out)

   private def readTermStructure(in: Input): TermStructure = 
       TermStructure.read(in)

   private def readAndAdoptTermStructure(in: Input, bc: BlockContext): BlockContext = 
   {
     val ts = TermStructure.read(in)
     bc withStructure ts      
   }

}

object Serializer extends Serializer
{
  def termSystem = FreeTermSystem
}
