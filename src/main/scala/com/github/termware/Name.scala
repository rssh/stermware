package com.github.termware

sealed trait Name

case class SymbolName(s:Symbol) extends Name
case class PrimitiveName[V](v:V) extends Name


// vim: set ts=4 sw=4 et:
