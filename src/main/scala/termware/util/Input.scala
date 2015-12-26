package termware.util

import java.nio.charset.StandardCharsets

trait Input
{

   def read(bytes: Array[Byte]):Int

   def readByte(): Byte

   def readInt():Int =
   {
     val v0 = readByte
     val v1 = readByte
     val v2 = readByte
     val v3 = readByte
     (((v0 << 8 + v1) << 8) + v2)<<8 + v3
   }

   def readChar():Char =
    readInt.toChar

   def readLong(): Long =
   {
     val v0 = readInt
     val  v1 = readInt
     (v0.toLong << 32) | v1
   }

   def readDouble(): Double =
     java.lang.Double.longBitsToDouble(readLong)

   def readString(): String =
   {
     val bytes = readOpaque
     new String(bytes,StandardCharsets.UTF_8)
   }
   
   def readOpaque(): Array[Byte] =
   {
     val length = readInt
     val bytes = new Array[Byte](length)
     val n = read(bytes)
     if (n<length) {
       throw new IllegalStateException("Unexpected end of stream")
     }
     bytes
   }

}
