package termware.free

import java.nio._
import termware._
import termware.util._
import termware.TermAttributes.{empty=>emptyAttributes}

// TODO: write input pos on error.

object Serializer extends TermSerializer
{

   def apply(t: Term, out: Output): Unit = 
   { writeTerm(t,BlockContext(),out) }

   def unapply(in: Input): Term =
    readTerm(in,BlockContext())._1

   case class BlockContext(
     val structures: Map[Int, TermStructure] = Map(),
     val invStructures: Map[TermStructure, Int] = Map(),
     val nextStructureIndex: Int = 0,
     val scopes:     Map[Int, Term] = Map(),
     val invScopes:  Map[Term,Int] = Map(),
     val nextScopeIndex: Int = 0
   ) {

     def withStructure(structure: TermStructure): (BlockContext, Int) =
     {
       invStructures.get(structure) match {
         case None => val si = nextStructureIndex
                      val nsi = si+1
                      (copy(structures = structures.updated(si, structure),
                            invStructures = invStructures.updated(structure,si),
                            nextStructureIndex = nsi), nsi)
         case Some(si) => (this, si)
       }
     }

     def retrieveStructure(i: Int): TermStructure =
       structures.get(i).getOrElse(throw new IllegalStateException("Invalid structure index"))

     def withScope(scope: Term): (BlockContext, Int) =
     {
       invScopes.get(scope) match {
         case None =>
              val si = nextScopeIndex
              val nsi = si+1
              (copy(scopes = scopes.updated(si,scope),
                    invScopes = invScopes.updated(scope,si),
                    nextScopeIndex = nsi), nsi)
         case Some(si) => (this, si)
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
        case TAG_ATOM => (AtomTerm(in.readString,Map()),bc)
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
       case StringTerm(v,_) => out << v
       case CharTerm(v,_)   => out << v
       case Int32Term(v,_)  => out << v
       case Int64Term(v,_)  => out << v
       case DoubleTerm(v,_) => out << v
       case OpaqueTerm(v,_) => (out << v.size).write(v)
     }
   }

   private def readPrimitive(tag: Int, in: Input): PrimitiveTerm =
   {
      import NameTypeIndexes._
      (tag & (~TAG_PRIMITIVE)) match {
        case STRING =>  StringTerm(in.readString,Map())
        case CHAR   =>  CharTerm(in.readChar,Map())
        case INT    =>  Int32Term(in.readInt,Map())
        case LONG   =>  Int64Term(in.readLong,Map())
        case DOUBLE =>  DoubleTerm(in.readDouble(),Map()) 
        case OPAQUE =>  OpaqueTerm(in.readOpaque(),Map())       
        case _ => throw new IllegalStateException("Invalid primitive tag: "+tag)
      }
   }
 
   private def writeStructured(t: StructuredTerm, bc: BlockContext, out: Output): BlockContext =
   {
      val ts = t.termStructure;
      val nextSti = bc.nextStructureIndex
      val (nbc0, sti) = bc.withStructure(ts)
      if (nextSti == sti) {
          out.writeInt(TAG_TERMSTRUCTURE) 
          out.writeInt(sti)
          writeTermStructure(ts,out)
      }
      val (nbc, sci) = if (t.isScope) {
                          nbc0 withScope t
                       } else (nbc0, -1)
      out.writeInt(TAG_STRUCTURED) 
      out.writeInt(sti)
      out.writeInt(sci)
      out.writeInt(t.arity)
      t.components.foldLeft(nbc) { (s,e) =>
         writeTerm(e,s,out)
      } 
   }

   private def readStructured(in: Input, bc: BlockContext): (Term, BlockContext) =
   {
     val ts = bc.retrieveStructure(in.readInt)
     val sci = in.readInt
     if (sci != -1) {
        ???
     }
     val arity = in.readInt 
     val subterms = new Array[Term](arity)
     var cbc = bc
     for(i <- (0 until arity)) {
        val (t, nbc) = readTerm(in, cbc)
        subterms(i)=t
        cbc = nbc
     }
     (StructuredTerm(ts,subterms.toIndexedSeq,emptyAttributes), cbc)
   }

   private def writeVar(t: VarTerm, bc: BlockContext, out: Output): BlockContext =
   {
     writeName(t.name, out)
     out.writeInt(t.varIndex)
     out.writeInt(t.scopeIndex)
     bc
   }

   private def readVar(in: Input, bc: BlockContext): VarTerm = 
   {
     val name = readName(in)                          
     val varIndex = in.readInt()
     val scopeIndex = in.readInt
     VarTerm(name=name,varIndex,scopeIndex,Map())
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
     (bc withStructure ts)._1
   }

}

