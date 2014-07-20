package termware

import scala.reflect.runtime.universe._

/**
 * reconstruct mapping from scala object to terms, as in default system
 */
object Reflect {
  
  def caseClassSubterms[X <: AnyRef with Product](x:X, xtag: TypeTag[X]): IndexedSeq[Term] =
  {
    ???
  }
  
  @specialized
  def asTerm[X](x:X, xtag: TypeTag[X]): Term =
  {
    val ts = xtag.tpe.typeSymbol
    if (ts.isClass) {
      val cs = ts.asClass
      if (cs.isPrimitive) {
        if (cs.isNumeric) {
          if (xtag.tpe =:= typeOf[Long]) {
            AsTerm.create[Long](x.asInstanceOf[Long])
          } else if (xtag.tpe =:= typeOf[Int]) {
            AsTerm.create[Int](x.asInstanceOf[Int])
          } else if (xtag.tpe =:= typeOf[Float]) {
            AsTerm.create[Float](x.asInstanceOf[Float])
          } else {
            ???
          }
        } else if (xtag.tpe =:= typeOf[Char]) {
          AsTerm.create[Char](x.asInstanceOf[Char])
        } else if (xtag.tpe =:= typeOf[Boolean]) {
          AsTerm.create[Boolean](x.asInstanceOf[Boolean])
        } else if (xtag.tpe =:= typeOf[Unit]) {
         /// AsTerm.create[Unit](())
          ???
        } else {
          throw new IllegalArgumentException("Unknown primitive:"+cs)
        }
      } else if (cs.isCaseClass) {
        val xxtag = xtag.asInstanceOf[TypeTag[({ type Y <: AnyRef with Product })#Y ]]
        val cct = new CaseClassToTerm[({ type Y <: AnyRef with Product })#Y]()(xxtag)
        new AsTerm(x.asInstanceOf[({ type Y <: AnyRef with Product })#Y], cct)(xxtag)
      } else if (xtag.tpe =:= typeOf[String]) {
        AsTerm.create[String](x.asInstanceOf[String])
      } else {
        ???
        // TODO: AnyRef term.
      }
    } else {
      ???
    }
  }
    

}
